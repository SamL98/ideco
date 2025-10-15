use pyo3::prelude::*;
use pyo3::types::{PyInt, PyString, PyBool, PyTuple, PyList, PyModule};

use std::fmt::{self, Debug};
use std::cell::RefCell;
use std::rc::Rc;

use crate::template::{TemplateChild, TemplateContext};
use std::collections::{HashSet, HashMap};
use crate::pool::{NodeIndex, NodePool};
use crate::repr::*;
use crate::app::*;

#[derive(Clone)]
pub enum Token {
    Literal(String),
    Type(String),
}

pub struct NodeDescriptor {
    pub inner: Py<PyAny>,
    name: String,
    pub superclass: String,
    pub src_superclass: Option<String>,
    pub has_template: bool,
    pub is_hidden: bool,
    pub is_trivial: bool,
    pub is_seq: bool,
    pub passthrough: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub child_types: HashMap<String, String>,
    pub layout: Py<PyAny>,
    pub first_token: Token,
    pub priority: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanguageModule {
    pub name: String,
    pub descriptors: HashMap<String, NodeDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Language {
    pub modules: HashMap<String, LanguageModule>,
}

#[allow(deprecated)]
impl Language {
    pub fn load(modules: &[String]) -> Self {
        Python::with_gil(|py| {
            let mut lang = Language::new();

            let sys = py.import("sys").unwrap();

            let module = PyModule::new_bound(py, "ideco").unwrap();
            module.add_function(wrap_pyfunction!(eval_repr, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(eval_tmpl, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(set_type, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(propagate, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(get_data_type, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(find_all, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(make_node, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(make_seq, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(get_seq, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(get_parents, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(get_function, &module).unwrap()).unwrap();
            module.add_function(wrap_pyfunction!(register, &module).unwrap()).unwrap();

            sys.getattr("modules").unwrap().set_item("ideco", module).unwrap();

            let path = sys.getattr("path").unwrap();
            let path_list = path.downcast::<PyList>().unwrap();

            let current_dir = std::env::current_dir().unwrap();
            let current_dir_str = current_dir.to_string_lossy().to_string();
            path_list.append(current_dir_str).unwrap();
            path_list.insert(0, current_dir.join("languages").to_string_lossy().to_string()).unwrap();

            let languages_module = py.import_bound("languages").unwrap();
            let load_module = languages_module.getattr("load_module").unwrap();

            let mod_name = "tmp_module";
            let tmp_module = PyModule::new(py, mod_name).unwrap();
            sys.getattr("modules").unwrap().set_item(mod_name, tmp_module).unwrap();
            // tmp_module.setattr("ideco", module);

            // Have Python parse the node type tree
            for module_name in &["base", "hlil"] {
                let mut module = LanguageModule::new(module_name);
                let full_name = format!("{}", module_name);

                match load_module.call((full_name,), None) {
                    Ok(node_types) => {
                        for node_type in node_types.iter().unwrap() {
                            let desc = NodeDescriptor::new(node_type.unwrap());
                            module.add_descriptor(desc.name().to_string(), &desc);
                        }
                    },
                    Err(err) => panic!("{}", err),
                }

                lang.add_module(module_name, module);
            }

            for module_name in modules {
                let mut module = LanguageModule::new(module_name);
                let full_name = format!("{}", module_name);

                match load_module.call((full_name,), None) {
                    Ok(node_types) => {
                        for node_type in node_types.iter().unwrap() {
                            let desc = NodeDescriptor::new(node_type.unwrap());
                            module.add_descriptor(desc.name().to_string(), &desc);
                        }
                    },
                    Err(err) => panic!("{}", err),
                }

                lang.add_module(module_name, module);
            }

            lang
        })
    }
}

#[allow(deprecated)]
impl Clone for NodeDescriptor {
    fn clone(&self) -> Self {
        Python::with_gil(|py| {
            NodeDescriptor {
                inner: self.inner.call_method(py, "copy", (&self.inner,), None).unwrap(),
                name: self.name.clone(),
                superclass: self.superclass.clone(),
                src_superclass: self.src_superclass.clone(),
                has_template: self.has_template,
                is_hidden: self.is_hidden,
                is_trivial: self.is_trivial,
                is_seq: self.is_seq,
                passthrough: self.passthrough.clone(),
                inputs: self.inputs.clone(),
                outputs: self.outputs.clone(),
                child_types: self.child_types.clone(),
                layout: self.inner.call_method0(py, "data_layout").unwrap(),
                first_token: self.first_token.clone(),
                priority: self.priority,
            }
        })
    }
}

impl PartialEq for NodeDescriptor {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Eq for NodeDescriptor {}

impl Debug for NodeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NodeDescriptor({})", self.name())
    }
}

pub struct Wrapper<T> {
    pub inner: Rc<RefCell<T>>,
}

unsafe impl<T> Send for Wrapper<T> {}
unsafe impl<T> Sync for Wrapper<T> {}

#[pyclass]
pub struct PyPoolWrapper {
    pub inner: Wrapper<NodePool>,
}

#[pyclass]
pub struct PyLangWrapper {
    pub inner: Wrapper<Language>,
}

#[pyclass]
pub struct PyReprCtxWrapper {
    pub inner: Wrapper<ReprContext>,
}

#[pyclass]
pub struct PyTmplCtxWrapper {
    pub inner: Wrapper<TemplateContext>,
}

#[pyclass]
pub struct PyFoldedExprsWrapper {
    pub inner: Wrapper<HashSet<NodeIndex>>,
}

#[pyclass]
pub struct PyChildrenWrapper {
    pub inner: Wrapper<HashMap<String, TemplateChild>>,
}

#[pyclass]
pub struct PyDescriptorInstance {
    #[pyo3(get)]
    pub attrs: HashMap<String, Py<PyAny>>,
}

#[pymethods]
impl PyDescriptorInstance {
    fn __getattr__(&self, attr: &str) -> Py<PyAny> {
        Python::with_gil(|py| {
            //println!("{} {:?}", attr, self.attrs.keys());
            if let Some(value) = self.attrs.get(&attr.to_string()) {
                value.bind(py).clone().unbind()
            } else {
                panic!("No attribute {}", attr)
            }
        })
    }

    pub fn set_attr(&mut self, attr: &str, val: Py<PyAny>) {
        self.attrs.insert(attr.to_string(), val);
    }
}

pub struct NodeInstance {
    pub inner: Py<PyAny>,
}

impl NodeInstance {
    pub fn set_attr(&mut self, attr: &str, val: Py<PyAny>) {
        let _ = Python::with_gil(|py| {
            //println!("{} {:?}", attr, self.attrs.keys());
            let py_inst = self.inner.bind(py).downcast::<PyDescriptorInstance>().unwrap();
            let _ = py_inst.call_method("set_attr", (attr, val), None);
        });
    }
}

impl Clone for NodeInstance {
    fn clone(&self) -> Self {
        Python::with_gil(|py| {
            let inner = self.inner.bind(py);
            let copy = inner.clone();

            Self {
                inner: copy.unbind(),
            }
        })
    }
}

impl PartialEq for NodeInstance {
    fn eq(&self, _other: &Self) -> bool {
        true
        // Python::with_gil(|py| {
        //     self.inner.call_method(py, "__eq__", (&other.inner), None).unwrap().downcast::<PyBool>(py).unwrap().is_true()
        // })
    }
}

impl Eq for NodeInstance {}

impl Debug for NodeInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NodeInstance")
    }
}

#[allow(deprecated)]
impl NodeDescriptor {
    pub fn new(inner: Bound<PyAny>) -> Self {
        let name = inner.getattr("__name__").unwrap().str().unwrap().to_string();
        let superclass = inner.call_method0("superclass").unwrap().str().unwrap().to_string();
        let mut src_superclass = Some(inner.call_method0("src_superclass").unwrap().str().unwrap().to_string());
        if src_superclass == Some("None".to_string()) {
            src_superclass = None;
        }
        let has_tmpl = inner.hasattr("match").unwrap();
        let is_hidden = inner.call_method0("is_hidden").unwrap().downcast::<PyBool>().unwrap().is_true();
        let is_trivial = inner.call_method0("is_trivial").unwrap().downcast::<PyBool>().unwrap().is_true();
        let is_seq = inner.call_method0("is_seq").unwrap().downcast::<PyBool>().unwrap().is_true();
        let passthrough = inner.call_method0("passthrough").unwrap().str().unwrap().to_string();
        let priority = inner.call_method0("priority").unwrap().downcast::<PyInt>().unwrap().extract().unwrap();

        let mut inputs = vec![];
        let mut outputs = vec![];

        for typ in inner.call_method0("inputs").unwrap().iter().unwrap() {
            inputs.push(typ.unwrap().downcast::<PyString>().unwrap().str().unwrap().to_string());
        }

        for typ in inner.call_method0("outputs").unwrap().iter().unwrap() {
            outputs.push(typ.unwrap().downcast::<PyString>().unwrap().str().unwrap().to_string());
        }

        let mut child_types = HashMap::default();
        let py = inner.py();

        for anno in inner.call_method0("child_types").unwrap().iter().unwrap() {
            let anno_ = anno.unwrap();
            let anno_tuple = anno_.downcast::<PyTuple>().unwrap();
            let child_name = anno_tuple.get_item(0).unwrap().downcast::<PyString>().unwrap().str().unwrap().to_string();
            let child_type = anno_tuple.get_item(1).unwrap().downcast::<PyString>().unwrap().str().unwrap().to_string();
            child_types.insert(child_name, child_type);
        }

        let layout = inner.call_method0("data_layout").unwrap();
        let layout = layout.into_py(py);

        let first_token_str = inner.call_method0("first_token").unwrap().str().unwrap().to_string();

        let first_token = if first_token_str.starts_with("type:") {
            Token::Type(first_token_str[5..].to_string())
        } else {
            Token::Literal(first_token_str.clone())
        };

        NodeDescriptor {
            inner: inner.into_py(py),
            name,
            superclass,
            src_superclass,
            has_template: has_tmpl,
            is_hidden,
            is_trivial,
            is_seq,
            passthrough,
            inputs,
            outputs,
            child_types,
            layout,
            first_token,
            priority,
        }
    }

    pub fn create_instance(&self, idx: usize, attrs: &HashMap<String, AttrVal>, pool: &NodePool) -> NodeInstance {
        Python::with_gil(|py| {
            let mut inst_attrs = HashMap::new();

            for (name, attr) in attrs {
                let val = match attr {
                    AttrVal::Num(n) => (*n).into_py(py),
                    AttrVal::Str(s) => s.into_py(py),
                    AttrVal::Node(ix) => pool.convert_to_py(*ix),
                };

                inst_attrs.insert(name.clone(), val);
            }

            inst_attrs.insert("node_type".to_string(), self.name.as_str().into_py(py));
            inst_attrs.insert("node_index".to_string(), idx.into_py(py));
            inst_attrs.insert("is_hidden".to_string(), self.is_hidden.into_py(py));

            if !inst_attrs.contains_key(&"data_type".to_string()) {
                inst_attrs.insert("data_type".to_string(), ().into_py(py));
            }

            let inst = Py::new(py, PyDescriptorInstance { attrs: inst_attrs }).unwrap();
            NodeInstance { inner: inst.into_any() }
        })
    }

    pub fn create_instance_ref(&self, idx: usize, attrs: &HashMap<String, AttrVal>, pool: Rc<RefCell<NodePool>>) -> NodeInstance {
        Python::with_gil(|py| {
            let mut inst_attrs = HashMap::new();

            for (name, attr) in attrs {
                let val = match attr {
                    AttrVal::Num(n) => (*n).into_py(py),
                    AttrVal::Str(s) => s.into_py(py),
                    AttrVal::Node(ix) => pool.borrow().convert_to_py(*ix),
                };

                inst_attrs.insert(name.clone(), val);
            }

            inst_attrs.insert("node_type".to_string(), self.name.as_str().into_py(py));
            inst_attrs.insert("node_index".to_string(), idx.into_py(py));
            inst_attrs.insert("is_hidden".to_string(), self.is_hidden.into_py(py));

            if !inst_attrs.contains_key(&"data_type".to_string()) {
                inst_attrs.insert("data_type".to_string(), ().into_py(py));
            }

            let inst = Py::new(py, PyDescriptorInstance { attrs: inst_attrs }).unwrap();
            NodeInstance { inner: inst.into_any() }
        })
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn superclass(&self) -> Option<&String> {
        Some(&self.superclass)
    }

    pub fn descends_from(&self, base_type: &str) -> bool {
        Python::with_gil(|py| {
            self.inner.call_method(py, "descends_from", (base_type,), None)
                .unwrap()
                .downcast_bound::<PyBool>(py)
                .unwrap()
                .is_true()
        })
	}

    pub fn references(&self, _idx: NodeIndex, _pool: Rc<RefCell<NodePool>>) -> Vec<u64> {
        vec![]
        // Python::with_gil(|py| {
        //     let params = Py::new(py, PyChildWrapper {
        //         inner: ChildWrapper {
        //             idx: idx,
        //             pool: pool,
        //         }
        //     }).unwrap();

        //     let mut refs = vec![];
        //     let it_ = self.inner.call_method(py, "references", (params,), None).unwrap();
        //     let it = it_.downcast_bound::<PyList>(py).unwrap();

        //     for r in it.iter() {
        //         refs.push(r.downcast::<PyInt>().unwrap().extract().unwrap());
        //     }

        //     refs
        // })
    }

    pub fn evaluate_repr_with(&self, inst: &NodeInstance, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>, folded_exprs: Rc<RefCell<HashSet<NodeIndex>>>) -> String {
        Python::with_gil(|py| {
            let sys = py.import("sys").unwrap();
            let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();

            let _ = ideco.setattr("pool", PyPoolWrapper { inner: Wrapper { inner: pool } }.into_py(py));
            let _ = ideco.setattr("ctx", PyReprCtxWrapper { inner: Wrapper { inner: ctx } }.into_py(py));
            let _ = ideco.setattr("folded_exprs", PyFoldedExprsWrapper { inner: Wrapper { inner: folded_exprs } }.into_py(py));

            self.inner.call_method(py, "__repr__", (inst.clone().inner,), None)
                .unwrap()
                .downcast_bound::<PyString>(py)
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
        })
    }
}

impl LanguageModule {
    pub fn new(name: &str) -> LanguageModule {
        LanguageModule {
            name: name.to_string(),
            descriptors: HashMap::default(),
        }
    }

    pub fn add_descriptor(&mut self, _name: String, desc: &NodeDescriptor) {
        // println!("  {}", name);
        self.descriptors.insert(desc.name().to_string(), desc.clone());
    }
}

impl Language {
    pub fn new() -> Language {
        Language {
            modules: HashMap::default(),
        }
    }

    pub fn add_module(&mut self, name: &str, module: LanguageModule) {
        // println!("{}", name);
        self.modules.insert(name.to_string(), module);
    }

    pub fn descriptor_for<'a>(&'a self, name: &str) -> &'a NodeDescriptor {
        // println!("{}", name);
        let comps: Vec<&str> = name.split(".").collect();
        let module = comps[0];
        let node_name = comps[1];
        &self.modules.get(module).map(|m| m.descriptors.get(node_name)).flatten().as_ref().unwrap()
    }
}
