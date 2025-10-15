use pyo3::prelude::*;

use crate::hlil::{Opcode, HlilNode, HlilKind};
use crate::lang::*;
use crate::repr::AttrVal;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::Ref;
use std::cell::RefCell;

pub type NodeIndex = usize;

// #[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[pyclass]
pub struct NodePool {
    pub items: Vec<HlilNode>,
    pub datas: HashMap<u64, NodeIndex>,
    pub vars: HashMap<String, NodeIndex>,
}

impl NodePool {
    pub fn new() -> Self {
        NodePool {
            items: vec![],
            datas: HashMap::default(),
            vars: HashMap::default(),
        }
    }

    pub fn get<'a>(&'a self, idx: NodeIndex) -> &'a HlilNode {
        &self.items[idx]
    }

    pub fn get_mut<'a>(&'a mut self, idx: NodeIndex) -> &'a mut HlilNode {
        &mut self.items[idx]
    }

    pub fn replace(&mut self, old_idx: NodeIndex, new_idx: NodeIndex) {
        let mut old_parents = self.items[old_idx].parents.clone();
        self.items[old_idx] = self.items[new_idx].clone();
        self.items[old_idx].parents.append(&mut old_parents);
        self.items[old_idx].idx = old_idx;
    }

    pub fn set_type(&mut self, idx: NodeIndex, dt: &str, lang: &RefCell<Language>) {
        let dt_idx = make_data_type(self, dt.to_string(), 8, lang); // FIXME: size
        let dt = AttrVal::Node(dt_idx);
        self.items[idx].attrs.insert("data_type".to_string(), dt);

        let dt_py = self.convert_to_py(dt_idx);

        if let Some(inst) = self.items[idx].desc_inst.as_mut() {
            inst.set_attr("data_type", dt_py);
        }
    }

    pub fn convert_to_py(&self, idx: NodeIndex) -> Py<PyAny> {
        Python::with_gil(|py| {
            if let Some(child_inst) = &self.items[idx].desc_inst {
                child_inst.clone().inner
            } else if let HlilKind::Seq(idxs) = &self.items[idx].kind {
                idxs.iter().map(|i| self.convert_to_py(*i)).collect::<Vec<Py<PyAny>>>().into_py(py)
            } else if let HlilKind::Int(val) = &self.items[idx].kind {
                (*val).into_py(py)
            } else {
                "INVALID".into_py(py)
            }
        })
    }
}

#[allow(dead_code)]
pub fn get_int(pool: &NodePool, idx: NodeIndex) -> Option<u64> {
    match &pool.items[idx].kind {
        HlilKind::Int(i) => Some(*i),
        _ => None,
    }
}

pub fn get_seq(pool: &Ref<NodePool>, idx: NodeIndex) -> Option<Vec<NodeIndex>> {
    match &pool.items[idx].kind {
        HlilKind::Seq(es) => Some(es.clone()),
        _ => None,
    }
}

#[allow(dead_code)]
pub fn get_tree(pool: &NodePool, idx: NodeIndex) -> Option<HashMap<String, NodeIndex>> {
    match &pool.items[idx].kind {
        HlilKind::Tree(t) => Some(t.clone()),
        _ => None,
    }
}

#[allow(dead_code)]
pub fn get_tree_ref(pool: Rc<RefCell<NodePool>>, idx: NodeIndex) -> Option<HashMap<String, NodeIndex>> {
    let p = pool.clone();
    let pb = (*p).borrow();

    match &pb.items[idx].kind {
        HlilKind::Tree(t) => Some(t.clone()),
        _ => None,
    }
}

pub fn deep_copy(pool: &mut NodePool, old_idx: NodeIndex, old_pool: Rc<RefCell<NodePool>>, var_map: &mut HashMap<NodeIndex, NodeIndex>, rename: bool) -> NodeIndex {
    use HlilKind::*;

    let p = old_pool.clone();
    let pb = (*p).borrow();
    let node = pb.get(old_idx);

    let new_kind = match &node.kind {
        Int(int) => Int(*int),
        Float(float) => Float(*float),
        Opcode(op) => Opcode(op.clone()),
        Str((s, addr)) => Str((s.clone(), *addr)),
        Var(name) => {
            var_map.insert(pool.items.len(), old_idx);
            let new_name = if rename {
                format!("_{}", name)
            } else {
                name.clone()
            };
            Var(new_name)
        },
        Data((addr, sym)) => Data((*addr, sym.clone())),
        Seq(elems) => {
            let s = elems.iter().map(|e| {
                let elem_idx = deep_copy(pool, *e, old_pool.clone(), var_map, rename);
                elem_idx
            }).collect::<Vec<NodeIndex>>();

            let idx = pool.items.len();

            for e in &s {
                pool.items[*e].parents.push(idx);
            }

            Seq(s)
        },
        Tree(tree) => {
            let mut children = HashMap::default();
            for (k, v) in tree {
                let child_idx = deep_copy(pool, *v, old_pool.clone(), var_map, rename);
                children.insert(k.clone(), child_idx);
            }

            let idx = pool.items.len();

            for (_, v) in &children {
                pool.items[*v].parents.push(idx);
            }

            Tree(children)
        }
    };

    let idx = pool.items.len();

    pool.items.push(HlilNode {
        idx: idx,
        kind: new_kind,
        addr: node.addr.clone(),
        attrs: node.attrs.clone(),
        name: node.name.clone(),
        parents: vec![],
        descriptor: node.descriptor.clone(),
        desc_inst: node.desc_inst.clone(),
        is_hidden: node.is_hidden,
    });

    idx
}

#[allow(dead_code)]
pub fn deep_copy_ref(
    pool: Rc<RefCell<NodePool>>,
    old_idx: NodeIndex,
    old_pool: Rc<RefCell<NodePool>>,
    var_map: &mut HashMap<NodeIndex, NodeIndex>,
    rename: bool,
) -> NodeIndex {
    use HlilKind::*;

    let p = old_pool.clone();
    let pb = (*p).borrow();
    let node = pb.get(old_idx);

    let new_kind = match &node.kind {
        Int(int) => Int(*int),
        Float(float) => Float(*float),
        Opcode(op) => Opcode(op.clone()),
        Str((s, addr)) => Str((s.clone(), *addr)),
        Var(name) => {
            var_map.insert(pool.borrow().items.len(), old_idx);
            let new_name = if rename {
                format!("_{}", name)
            } else {
                name.clone()
            };
            Var(new_name)
        },
        Data((addr, sym)) => Data((*addr, sym.clone())),
        Seq(elems) => {
            let s = elems.iter().map(|e| {
                let elem_idx = deep_copy_ref(pool.clone(), *e, old_pool.clone(), var_map, rename);
                elem_idx
            }).collect::<Vec<NodeIndex>>();

            let idx = pool.borrow().items.len();

            for e in &s {
                pool.borrow_mut().items[*e].parents.push(idx);
            }

            Seq(s)
        },
        Tree(tree) => {
            let mut children = HashMap::default();
            for (k, v) in tree {
                let child_idx = deep_copy_ref(pool.clone(), *v, old_pool.clone(), var_map, rename);
                children.insert(k.clone(), child_idx);
            }

            let idx = pool.borrow().items.len();

            for (_, v) in &children {
                pool.borrow_mut().items[*v].parents.push(idx);
            }

            Tree(children)
        }
    };

    let idx = pool.borrow().items.len();

    pool.borrow_mut().items.push(HlilNode {
        idx: idx,
        kind: new_kind,
        addr: node.addr.clone(),
        attrs: node.attrs.clone(),
        name: node.name.clone(),
        parents: vec![],
        descriptor: node.descriptor.clone(),
        desc_inst: node.desc_inst.clone(),
        is_hidden: node.is_hidden,
    });

    idx
}

pub fn make_int(pool: &mut NodePool, int: u64) -> NodeIndex {
    let idx = pool.items.len();
    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Int(int),
        addr: None,
        attrs: HashMap::default(),
        name: None,
        parents: vec![],
        descriptor: None,
        desc_inst: None,
        is_hidden: false,
    });
    idx
}

pub fn make_float(pool: &mut NodePool, float: f64) -> NodeIndex {
    let idx = pool.items.len();
    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Float(float),
        addr: None,
        attrs: HashMap::default(),
        name: None,
        parents: vec![],
        descriptor: None,
        desc_inst: None,
        is_hidden: false,
    });
    idx
}

pub fn make_opcode(pool: &mut NodePool, opc: Opcode) -> NodeIndex {
    let idx = pool.items.len();
    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Opcode(opc),
        addr: None,
        attrs: HashMap::default(),
        name: None,
        parents: vec![],
        descriptor: None,
        desc_inst: None,
        is_hidden: false,
    });
    idx
}

pub fn make_var(pool: &mut NodePool, name: String, data_type: AttrVal, lang: &RefCell<Language>) -> NodeIndex {
    if let Some(existing_idx) = pool.vars.get(&name) {
        return *existing_idx;
    }

    let mut attrs = HashMap::default();
    attrs.insert("data_type".to_string(), data_type);
    attrs.insert("name".to_string(), AttrVal::Str(name.clone()));

    let idx = pool.items.len();

    let desc = (*lang).borrow().descriptor_for("hlil.Var").clone();
    let inst = desc.create_instance(idx, &attrs, pool);

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Var(name.clone()),
        addr: None,
        attrs: attrs,
        name: Some("hlil.Var".to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden: false,
    });

    pool.vars.insert(name, idx);
    idx
}

pub fn make_data_type(pool: &mut NodePool, s: String, size: usize, lang: &RefCell<Language>) -> NodeIndex {
    let mut attrs = HashMap::default();
    attrs.insert("size".to_string(), AttrVal::Num(size as i64));
    attrs.insert("name".to_string(), AttrVal::Str(s.clone()));

    let idx = pool.items.len();

    let desc = (*lang).borrow().descriptor_for("base.DataType").clone();
    let inst = desc.create_instance(idx, &attrs, pool);

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Str((s, 0)),
        addr: None,
        attrs,
        name: Some("base.DataType".to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden: false,
    });
    idx
}

#[allow(dead_code)]
pub fn make_label(pool: &mut NodePool, s: String, lang: &RefCell<Language>) -> NodeIndex {
    let attrs = HashMap::default();
    let desc = (*lang).borrow().descriptor_for("hlil.Label").clone();

    let idx = pool.items.len();

    let inst = desc.create_instance(idx, &attrs, pool);

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Str((s, 0)),
        addr: None,
        attrs,
        name: Some("hlil.Label".to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden: false,
    });
    idx
}

pub fn make_str(pool: &mut NodePool, s: String, addr: u64, lang: &RefCell<Language>) -> NodeIndex {
    let s = s.replace("…", "...");

    let mut attrs = HashMap::default();
    attrs.insert("orig_str".to_string(), AttrVal::Str(s.clone()));
    let idx = pool.items.len();

    let desc = (*lang).borrow().descriptor_for("hlil.String").clone();
    let inst = desc.create_instance(idx, &attrs, pool);

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Str((s, addr)),
        addr: None,
        attrs,
        name: Some("hlil.String".to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden: false,
    });
    idx
}

pub fn make_data(pool: &mut NodePool, addr: u64, sym: String, node_type: &str, lang: &RefCell<Language>) -> NodeIndex {
    if let Some(existing_idx) = pool.datas.get(&addr) {
        return *existing_idx;
    }

    let sym = sym.replace("…", "...");

    let mut attrs = HashMap::default();
    attrs.insert("name".to_string(), AttrVal::Str(sym.clone()));

    let idx = pool.items.len();

    let desc = (*lang).borrow().descriptor_for(node_type).clone();
    let inst = desc.create_instance(idx, &attrs, pool);

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Data((addr, sym)),
        addr: None,
        attrs,
        name: Some(node_type.to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden: false,
    });

    pool.datas.insert(addr, idx);
    idx
}

pub fn make_seq(pool: &mut NodePool, elems: Vec<NodeIndex>) -> NodeIndex {
    let idx = pool.items.len();

    for elem in &elems {
        pool.items[*elem].parents.push(idx);
    }

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Seq(elems),
        addr: None,
        attrs: HashMap::default(),
        name: None,
        parents: vec![],
        descriptor: None,
        desc_inst: None,
        is_hidden: false,
    });

    idx
}

pub fn make_tree(pool: &mut NodePool, name: &str, children: HashMap<String, NodeIndex>, lang: &RefCell<Language>, addr: u64) -> NodeIndex {
    let idx = pool.items.len();

    let mut attrs = HashMap::default();

    for (name, child) in &children {
        pool.items[*child].parents.push(idx);
        attrs.insert(name.clone(), AttrVal::Node(*child));
    }

    let desc = (*lang).borrow().descriptor_for(name).clone();
    let inst = desc.create_instance(idx, &attrs, pool);
    let is_hidden = desc.is_hidden;

    pool.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Tree(children),
        addr: Some(addr),
        attrs,
        name: Some(name.to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden,
    });
    idx
}

#[allow(dead_code)]
pub fn make_data_ref(pool: Rc<RefCell<NodePool>>, addr: u64, sym: String, node_type: &str, lang: &RefCell<Language>) -> NodeIndex {
    let idx = (*pool).borrow().items.len();

    let mut attrs = HashMap::default();
    attrs.insert("name".to_string(), AttrVal::Str(sym.clone()));

    let desc = (*lang).borrow().descriptor_for(node_type).clone();
    let inst = desc.create_instance_ref(idx, &attrs, pool.clone());
    let is_hidden = desc.is_hidden;

    (*pool).borrow_mut().items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Data((addr, sym)),
        addr: None,
        attrs: HashMap::default(),
        name: Some(node_type.to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden,
    });
    idx
}

pub fn make_seq_ref(pool: Rc<RefCell<NodePool>>, elems: Vec<NodeIndex>) -> NodeIndex {
    let p = pool.clone();
    let mut pb = (*p).borrow_mut();
    let idx = pb.items.len();

    for elem in &elems {
        pb.items[*elem].parents.push(idx);
    }

    pb.items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Seq(elems),
        addr: None,
        attrs: HashMap::default(),
        name: None,
        parents: vec![],
        descriptor: None,
        desc_inst: None,
        is_hidden: false,
    });
    idx
}

pub fn make_tree_ref(pool: Rc<RefCell<NodePool>>, name: &str, children: HashMap<String, NodeIndex>, lang: &RefCell<Language>, addr: u64) -> NodeIndex {
    let idx = (*pool).borrow().items.len();

    let mut attrs = HashMap::default();

    for (child_name, child) in &children {
        (*pool).borrow_mut().items[*child].parents.push(idx);
        attrs.insert(child_name.clone(), AttrVal::Node(*child));
    }

    let desc = (*lang).borrow().descriptor_for(name).clone();
    let inst = desc.create_instance_ref(idx, &attrs, pool.clone());
    let is_hidden = desc.is_hidden;

    (*pool).borrow_mut().items.push(HlilNode {
        idx: idx,
        kind: HlilKind::Tree(children),
        addr: Some(addr),
        attrs: HashMap::default(),
        name: Some(name.to_string()),
        parents: vec![],
        descriptor: Some(desc),
        desc_inst: Some(inst),
        is_hidden,
    });
    idx
}
