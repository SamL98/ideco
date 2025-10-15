use pyo3::prelude::*;
use pyo3::types::{PyList, PyTuple, PyDict};

use tinyjson::JsonValue;

use crate::hlil::HlilKind;
use crate::lang::*;
use crate::repr::*;
use crate::pool::*;
use crate::template::*;
use crate::serialization::default_language;
use crate::workspace::Workspace;
use crate::Args;

use std::collections::{HashSet, HashMap};
use std::cell::RefCell;
use std::rc::Rc;

#[allow(deprecated)]
#[pyfunction]
pub fn get_data_type(layout: Bound<PyAny>) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let lang = ideco.getattr("lang").unwrap().extract::<Py<PyLangWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let _layout = layout.downcast::<PyList>().unwrap();
        let mut counter = 0;

        for (_, module) in lang.borrow().modules.iter() {
            for (_, desc) in module.descriptors.iter() {
                let _desc_layout = desc.layout.downcast::<PyList>(py).unwrap();
                // let mut matches = layout.len() == desc_layout.len();
                counter += 1;

                // if matches {
                //     for (v1, v2) in layout.iter().zip(desc_layout.iter()) {
                //         let t1 = v1.downcast::<PyTuple>().unwrap();
                //         let t2 = v2.downcast::<PyTuple>().unwrap();

                //         let (n1, i1) = t1.extract::<(&str, PyAny)>().unwrap();
                //         let (n2, i2) = t2.extract::<(&str, PyAny)>().unwrap();

                //         let dt1 = i1.attrs[&"node_type".to_string()].bind(py).extract::<&str>().unwrap();
                //         let dt2 = i2.attrs[&"node_type".to_string()].bind(py).extract::<&str>().unwrap();

                //         if n1 != n2 || dt1 != dt2 {
                //             matches = false;
                //             break;
                //         }
                //     }
                // }

                // if matches {
                //     println!("======== Matched type {}", desc.name());
                //     return (desc.clone().inner, 0).to_object(py);
                // }
            }
        }

        ((), counter).to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn register(module_name: &str, py_desc: Bound<PyAny>) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let lang = ideco.getattr("lang").unwrap().extract::<Py<PyLangWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let desc = NodeDescriptor::new(py_desc);
        lang.borrow_mut().modules.get_mut(module_name).unwrap().add_descriptor(desc.name().to_string(), &desc);

        0.to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn get_parents(node_idx: usize) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let parents = (*pool).borrow().get(node_idx).parents.clone();
        parents.to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn get_seq(idx: usize) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let p = pool.clone();
        let pb = p.borrow();

        match &pb.items[idx].kind {
            HlilKind::Seq(es) => es.clone().to_object(py),
            _ => unreachable!(),
        }
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn make_seq(py_elems: Bound<PyAny>) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let mut elems = Vec::new();

        // for (k, v) in py_children.extract::<Py<PyDict>>().unwrap().borrow(py).iter() {
        for inst in py_elems.downcast::<PyList>().unwrap().iter() {
            // let idx = inst.getattr("node_index").unwrap().extract::<usize>().unwrap();
            let idx = inst.extract::<usize>().unwrap();
            elems.push(idx);
        }

        let idx = make_seq_ref(pool.clone(), elems);
        idx.to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn find_all(node_type: &str, py_inst: Bound<PyAny>) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let inst = py_inst.downcast::<PyDescriptorInstance>().unwrap();
        let idx = inst.getattr("node_index").unwrap().extract::<usize>().unwrap();

        let mut idxs = Vec::new();
        let mut visited = HashSet::new();
        (*pool).borrow().get(idx).find_all(&node_type.to_string(), pool.clone(), &mut idxs, &mut visited);

        let list = PyList::new_bound(py, idxs);
        list.into_any().unbind()
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn make_node(node_type: &str, addr: u64, py_children: Bound<PyAny>) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let lang = ideco.getattr("lang").unwrap().extract::<Py<PyLangWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let mut children = HashMap::new();

        // for (k, v) in py_children.extract::<Py<PyDict>>().unwrap().borrow(py).iter() {
        for (k, v) in py_children.downcast::<PyDict>().unwrap().iter() {
            let name = k.extract::<&str>().unwrap().to_string();
            let value = if let Ok(list) = v.downcast::<PyList>() {
                let elems: Vec<NodeIndex> = list.iter().map(|l| l.getattr("node_index").unwrap().extract::<usize>().unwrap()).collect();
                make_seq_ref(pool.clone(), elems)
            } else if let Ok(node_index) = v.getattr("node_index") {
                node_index.extract::<usize>().unwrap()
            } else {
                v.extract::<usize>().unwrap()
            };
            children.insert(name, value);
        }

        let idx = make_tree_ref(pool.clone(), node_type, children, &lang, addr);
        let child_inst = (*pool).borrow().items[idx].desc_inst.clone().unwrap();
        child_inst.inner.into_any()
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn get_function() -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let n = (*pool).borrow().items.len();

        for idx in 0..n {
            if (*pool).borrow().get(idx).descends_from("hlil.Function") {
                let func = (*pool).borrow().items[idx].desc_inst.clone().unwrap();
                return func.inner.into_any();
            }
        }

        panic!()
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn set_type(inst: Bound<PyDescriptorInstance>, dt: &str) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let lang = ideco.getattr("lang").unwrap().extract::<Py<PyLangWrapper>>().unwrap().borrow(py).inner.inner.clone();

        let idx = inst.getattr("node_index").unwrap().extract::<usize>().unwrap();
        (*pool).borrow_mut().set_type(idx, dt, &*lang);

        0.to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn propagate(lhs_idx: usize, rhs_idx: usize) -> PyResult<PyObject> {
    Ok(Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();

        // let lhs_idx = lhs.getattr("node_index").unwrap().extract::<usize>().unwrap();
        // let rhs_idx = rhs.getattr("node_index").unwrap().extract::<usize>().unwrap();
        // println!("Replacing {} with {}", (*pool).borrow().get(lhs_idx).str(pool.clone()), (*pool).borrow().get(rhs_idx).str(pool.clone()));

        (*pool).borrow_mut().replace(lhs_idx, rhs_idx);

        0.to_object(py)
    }))
}

#[allow(deprecated)]
#[pyfunction]
pub fn eval_repr(s: &str) -> String {
    let repr = Repr::from_str(s);

    Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let ctx = ideco.getattr("ctx").unwrap().extract::<Py<PyReprCtxWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let folded_exprs = ideco.getattr("folded_exprs").unwrap().extract::<Py<PyFoldedExprsWrapper>>().unwrap().borrow(py).inner.inner.clone();
        repr.evaluate_with(pool, ctx, folded_exprs)
    })
}

#[allow(deprecated)]
#[pyfunction]
pub fn eval_tmpl(s: &str) -> Py<PyAny> {
    let template = Template::from_str(s);

    Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let ideco = ideco.extract::<Bound<PyAny>>().unwrap();
        let pool = ideco.getattr("pool").unwrap().extract::<Py<PyPoolWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let ctx = ideco.getattr("ctx").unwrap().extract::<Py<PyTmplCtxWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let superclass = ideco.getattr("superclass").unwrap().extract::<&str>().unwrap();
        let child_types = ideco.getattr("child_types").unwrap().extract::<HashMap<String, String>>().unwrap();
        let children = ideco.getattr("children").unwrap().extract::<Py<PyChildrenWrapper>>().unwrap().borrow(py).inner.inner.clone();
        let start = ideco.getattr("start").unwrap().extract::<usize>().unwrap();

        let mut vars = HashMap::new();
        let mut ok = false;

        if let Some(end) = template.evaluate_with(start, children.clone(), pool.clone(), ctx, superclass, &child_types) {
            let _ = ideco.setattr("end", end.into_py(py));
            ok = true;

            for (n, c) in children.borrow().iter() {
                let p = pool.clone();
                let pb = (*p).borrow();

                vars.insert(n.clone(), match c {
                    TemplateChild::Index(idx) => {
                        match &pb.get(*idx).kind {
                            HlilKind::Int(int) => int.into_py(py),
                            _ => {
                                let child_inst = pb.items[*idx].desc_inst.clone().unwrap();
                                child_inst.inner
                            }
                        }
                    },
                    TemplateChild::Seq(idxs) => {
                        let mut elems = vec![];
                        for idx in idxs {
                            let elem = match &pb.get(*idx).kind {
                                HlilKind::Int(int) => int.into_py(py),
                                _ => {
                                    let child_inst = pb.items[*idx].desc_inst.clone().unwrap();
                                    child_inst.inner
                                }
                            };
                            elems.push(elem);
                        }
                        elems.into_py(py)
                    },
                });
            }
        }

        let tup = PyTuple::new_bound(py, [ok.into_py(py), vars.into_py(py)]);
        tup.into_any().unbind()
    })
}

#[allow(dead_code)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct App {
	pub workspaces: Vec<Workspace>,
    pub selected_idx: usize,

	#[serde(skip)]
	#[serde(default = "default_language")]
	pub lang: Rc<RefCell<Language>>,

	#[serde(skip)]
	pub has_run_init: bool,

	#[serde(skip)]
    pub name2addr: HashMap<String, u64>,

	#[serde(skip)]
    pub addr2func: HashMap<u64, HashMap<String, JsonValue>>,
}

//impl App {
//	pub fn do_bullshit_serde_init(&mut self) {
//		// HACK: I don't want to have to deal with all this serde DeserializerSeed/Visitor bullshit
//		//		 so just default anything that would need that to None or its equivalent and
//		//		 set their values right after deseralizing.
//		if self.has_run_init {
//			return;
//		}

//        let l = self.lang.clone();
//        let lb = (*l).borrow();


//        for ws in &mut self.workspaces {
//            for func in &mut ws.functions {
//                {
//                    let p = func.pool.clone();
//                    let mut pb = (*p).borrow_mut();

//                    for node in &mut pb.items {
//                        if let Some(name) = &node.name {
//                            node.descriptor = Some(lb.descriptor_for(name.as_str()).clone());
//                        }
//                    }
//                }
//            }
//        }

//		self.has_run_init = true;
//	}
//}

impl App {
    pub fn new(args: Args) -> Self {
		let lang = Rc::new(RefCell::new(Language::load(&args.modules)));

        let json_text = std::fs::read_to_string(&args.path).unwrap();
        let json: JsonValue = json_text.parse().unwrap();
        let funcs: &Vec<_> = json.get().unwrap();

        let mut name2addr = HashMap::new();
        let mut addr2func = HashMap::new();

        for obj in funcs {
            let obj_json: &HashMap<_, _> = obj.get().unwrap();
            let obj_addr: f64 = *obj_json["address"].get().unwrap();
            let obj_name: String = obj_json["name"].clone().try_into().unwrap();

            name2addr.insert(obj_name, obj_addr as u64);
            addr2func.insert(obj_addr as u64, obj_json.clone());
        }

		let mut workspace = Workspace::new("Default Workspace", name2addr.clone(), addr2func.clone());
        let _ = workspace.open_function(args.addr, 0.0, 0.0, 0.0, 0.0, lang.clone(), None, true, true);

		App {
			workspaces: vec![workspace],
            selected_idx: 0,
            lang,
			has_run_init: false,
            name2addr,
            addr2func,
		}
    }
}
