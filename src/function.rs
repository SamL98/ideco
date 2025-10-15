use tinyjson::JsonValue;

use pyo3::prelude::*;
use pyo3::types::{PyList, PyBool};

use crate::log::log;
use crate::pool::*;
use crate::hlil::HlilNode;
use crate::lang::*;
use crate::repr::{ReprContext, AttrVal};
use crate::template::{TemplateContext, TemplateChild};
use crate::hlil::HlilKind;
use crate::serialization::{serialize_pool, deserialize_pool, serialize_folds, deserialize_folds};

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use std::cell::Ref;
use std::cell::RefCell;

pub struct TemplateWrapper {
    pub pool: Rc<RefCell<NodePool>>,
    pub lang: Rc<Language>,
    pub ctx: Rc<RefCell<TemplateContext>>,
    pub superclass: String,
    pub child_types: Rc<HashMap<String, String>>,
    pub children: Rc<RefCell<HashMap<String, TemplateChild>>>,
    pub min_start: Rc<RefCell<Option<usize>>>,
    pub start: Rc<RefCell<usize>>,
    pub matched_idxs: Rc<RefCell<Vec<NodeIndex>>>,
}

unsafe impl Sync for TemplateWrapper {}
unsafe impl Send for TemplateWrapper {}

#[pyclass]
pub struct PyTemplateWrapper {
    pub inner: TemplateWrapper,
}

#[allow(deprecated)]
pub fn get_matched_idxs(
    start: usize,
    end: usize,
    superclass: Option<&String>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    _lang: Rc<RefCell<Language>>,
) -> Vec<usize> {
    let c = ctx.clone();
    let cb = (*c).borrow();
    let rc = cb.repr_ctx.clone();
    let rcb = (*rc).borrow();
    let printer = &rcb.printer;

    println!("Matched something at ({} - {}) {}", start, end, &printer.raw_text[start..end]);

    if printer.raw_range_to_node.contains_key(&(start, end)) && superclass.is_some() {
        let superclass = superclass.unwrap();

        for old_idx in &printer.raw_range_to_node[&(start, end)] {
            if (*pool).borrow().get(*old_idx).descends_from(superclass) {
                return vec![*old_idx];
            }
        }

        println!("*** Could not find node that descends from {}", superclass);
        for old_idx in &printer.raw_range_to_node[&(start, end)] {
            println!("    {:?}: {}", (*pool).borrow().get(*old_idx).name, (*pool).borrow().get(*old_idx).str(pool.clone()));
        }

        return vec![];
    }

    let mut old_idxs = vec![];
    let mut tmp_start = start;

    while tmp_start < end {
        let mut tmp_end = end;

        while
            tmp_end > tmp_start &&
            (
                !printer.raw_range_to_node.contains_key(&(tmp_start, tmp_end)) ||
                (*pool).borrow().get(printer.raw_range_to_node[&(tmp_start, tmp_end)][0]).name.is_none()
            )
        {
            tmp_end -= 1;
        }

        if !printer.raw_range_to_node.contains_key(&(tmp_start, tmp_end)) {
            tmp_start += 1;
            continue;
        }

        let old_idx = *printer.raw_range_to_node[&(tmp_start, tmp_end)].last().unwrap();
        old_idxs.push(old_idx);
        tmp_start = tmp_end + 1;

    }

    old_idxs
}

pub fn get_seq_idx(
    old_idxs: &[usize],
    pool: Rc<RefCell<NodePool>>,
) -> usize {
    // let mut cands = HashMap::new();

    for old_idx in old_idxs {
        let node = (*pool).borrow().get(*old_idx).clone();

        for parent_idx in &node.parents {
            if let Some(seq) = get_seq(&(*pool).borrow(), *parent_idx) {
                if seq.iter().position(|i| i == old_idx).is_some() {
                    // *cands.entry(*parent_idx).or_insert(0) += 1;

                    let mut ok = true;
                    for elem_idx in old_idxs {
                        if seq.iter().position(|i| i == elem_idx).is_none() {
                            ok = false;
                        }
                    }

                    if !ok {
                        continue;
                    }

                    return *parent_idx;
                }
            }
        }
    }

    // for (parent_idx, num) in &cands {
    //     if *num == old_idxs.len() {
    //         return *parent_idx;
    //     }
    // }

    for idx in old_idxs {
        let node = (*pool).borrow().get(*idx).clone();
        println!("  {:?}: {}", node.name, node.str(pool.clone()));
    }

    for idx in old_idxs {
        let node = (*pool).borrow().get(*idx).clone();

        for parent_idx in &node.parents {
            if let Some(seq) = get_seq(&(*pool).borrow(), *parent_idx) {
                for elem in seq {
                    println!("    {}", (*pool).borrow().get(elem).str(pool.clone()));
                }
            }
            println!("    ========================");
        }
    }
    // for (parent_idx, num) in cands {
    //     println!("  {}", num);
    //     println!("{}", (*pool).borrow().items[parent_idx].str(pool.clone()));
    // }
    panic!("Could not find seq idx");
}

#[allow(deprecated)]
pub fn make_new_seq(
    seq_idx: NodeIndex,
    mut elem_idxs: Vec<NodeIndex>,
    replacements: &mut Vec<(NodeIndex, NodeIndex)>,
    node_type: &String,
    mut children: HashMap<String, NodeIndex>,
    pool: Rc<RefCell<NodePool>>,
    _ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) {
    let mut seq = {
        let p = pool.clone();
        let pb = (*p).borrow();
        get_seq(&pb, seq_idx).unwrap()
    };

    elem_idxs.sort();

    println!("Creating {} for seq", node_type);
    // for idx in &elem_idxs {
    //     println!("  {}", (*pool).borrow().items[*idx].str(pool.clone()));
    // }

    let leftovers = children.remove("leftovers");

    let addr = (*pool).borrow().get(elem_idxs[0]).addr.unwrap_or(0);
    let mut new_node_idx = make_tree_ref(pool.clone(), node_type.as_str(), children, &*lang, addr);

    let desc = (*pool).borrow().items[new_node_idx].descriptor.clone().unwrap();
    let inst = (*pool).borrow().items[new_node_idx].desc_inst.clone().unwrap();

    let _ = Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let _ = ideco.setattr("lang", PyLangWrapper { inner: Wrapper { inner: lang.clone() } }.into_py(py));
        let _ = ideco.setattr("pool", PyPoolWrapper { inner: Wrapper { inner: pool.clone() } }.into_py(py));
        desc.inner.call_method(py, "init", (inst.inner,), None)
    });

    if desc.passthrough.len() > 0 {
        new_node_idx = (*pool).borrow().get(new_node_idx).get_child(&desc.passthrough).unwrap();
    }

    let mut max_idx = 0;
    let mut min_idx = 1000000000;

    for elem_idx in elem_idxs {
        let idx = seq.iter().position(|i| *i == elem_idx).unwrap();
        min_idx = min_idx.min(idx);
        max_idx = max_idx.max(idx);
    }

    seq.drain(min_idx..=max_idx);
    seq.insert(min_idx, new_node_idx);

    if let Some(leftover_idx) = leftovers {
        for stmt_idx in get_seq(&(*pool).borrow(), leftover_idx).unwrap() {
            seq.insert(min_idx - 1, stmt_idx);
        }
    }

    let new_seq_idx = make_seq_ref(pool.clone(), seq);
    (*pool).borrow_mut().replace(seq_idx, new_seq_idx);
    replacements.push((seq_idx, new_seq_idx));
}

#[allow(deprecated)]
pub fn insert_new_seq(
    seq_idx: NodeIndex,
    mut elem_idxs: Vec<NodeIndex>,
    replacements: &mut Vec<(NodeIndex, NodeIndex)>,
    node_type: &String,
    children: HashMap<String, NodeIndex>,
    pool: Rc<RefCell<NodePool>>,
    _ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) {
    let mut seq = {
        let p = pool.clone();
        let pb = (*p).borrow();
        get_seq(&pb, seq_idx).unwrap()
    };

    elem_idxs.sort();

    println!("Creating seq {} for seq", node_type);
    // println!("{}", (*pool).borrow().items[seq_idx].str(pool.clone()));
    // for idx in &elem_idxs {
    //     println!("  {}", (*pool).borrow().items[*idx].str(pool.clone()));
    // }

    let addr = (*pool).borrow().get(elem_idxs[0]).addr.unwrap_or(0);
    let new_node_idx = make_tree_ref(pool.clone(), node_type.as_str(), children, &*lang, addr);

    let desc = (*pool).borrow().items[new_node_idx].descriptor.clone().unwrap();
    let inst = (*pool).borrow().items[new_node_idx].desc_inst.clone().unwrap();

    let new_idxs = Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let _ = ideco.setattr("lang", PyLangWrapper { inner: Wrapper { inner: lang.clone() } }.into_py(py));
        let _ = ideco.setattr("pool", PyPoolWrapper { inner: Wrapper { inner: pool.clone() } }.into_py(py));
        let _ = desc.inner.call_method(py, "init", (inst.clone().inner,), None);
        desc.inner.call_method(py, "seq", (inst.inner,), None).unwrap().downcast::<PyList>(py).unwrap().iter().map(|e| {
            e.extract::<usize>().unwrap()
        }).collect::<Vec<usize>>()
    });

    let mut max_idx = 0;
    let mut min_idx = 1000000000;

    for elem_idx in elem_idxs {
        let idx = seq.iter().position(|i| *i == elem_idx).unwrap();
        min_idx = min_idx.min(idx);
        max_idx = max_idx.max(idx);
    }

    seq.drain(min_idx..=max_idx);

    for new_idx in new_idxs.iter().rev() {
        // println!("  {}", (*pool).borrow().items[new_idx].str(pool.clone()));
        seq.insert(min_idx, *new_idx);
    }

    let new_seq_idx = make_seq_ref(pool.clone(), seq);
    // println!("{} --> {}", seq_idx, new_seq_idx);
    // println!("{}", (*pool).borrow().items[new_seq_idx].str(pool.clone()));
    (*pool).borrow_mut().replace(seq_idx, new_seq_idx);
    // println!("{}", (*pool).borrow().items[seq_idx].str(pool.clone()));
    // panic!();
    replacements.push((seq_idx, new_seq_idx));
}

#[allow(deprecated)]
pub fn make_new_tree(
    tree_idx: NodeIndex,
    replacements: &mut Vec<(NodeIndex, NodeIndex)>,
    node_type: &String,
    children: HashMap<String, NodeIndex>,
    pool: Rc<RefCell<NodePool>>,
    _ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) {
    // println!("Creating {} for tree {} of type {:?}", node_type, (*pool).borrow().get(tree_idx).str(pool.clone()), (*pool).borrow().get(tree_idx).name);
    let addr = (*pool).borrow().get(tree_idx).addr.unwrap_or(0);
    let mut new_tree_idx = make_tree_ref(pool.clone(), node_type.as_str(), children, &*lang, addr);

    let desc = (*pool).borrow().items[new_tree_idx].descriptor.clone().unwrap();
    let inst = (*pool).borrow().items[new_tree_idx].desc_inst.clone().unwrap();

    let _ = Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();
        let _ = ideco.setattr("lang", PyLangWrapper { inner: Wrapper { inner: lang.clone() } }.into_py(py));
        let _ = ideco.setattr("pool", PyPoolWrapper { inner: Wrapper { inner: pool.clone() } }.into_py(py));
        desc.inner.call_method(py, "init", (inst.inner,), None)
    });

    if desc.passthrough.len() > 0 {
        new_tree_idx = (*pool).borrow().get(new_tree_idx).get_child(&desc.passthrough).unwrap();
    }

    (*pool).borrow_mut().replace(tree_idx, new_tree_idx);
    replacements.push((tree_idx, new_tree_idx));
}

#[allow(deprecated)]
pub fn lift_desc_at(
    start: usize,
    desc: &NodeDescriptor,
    node_type: String,
    _root: NodeIndex,
    replacements: &mut Vec<(NodeIndex, NodeIndex)>,
    visited: &mut HashSet<(usize, usize)>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) -> Option<usize> {
    let mut end = None;

    Python::with_gil(|py| {
        let sys = py.import("sys").unwrap();
        let ideco = sys.getattr("modules").unwrap().get_item("ideco").unwrap();

        let children_ = HashMap::new();
        let children = Rc::new(RefCell::new(children_));

        let _ = ideco.setattr("pool", PyPoolWrapper { inner: Wrapper { inner: pool.clone() } }.into_py(py));
        let _ = ideco.setattr("lang", PyLangWrapper { inner: Wrapper { inner: lang.clone() } }.into_py(py));
        let _ = ideco.setattr("ctx", PyTmplCtxWrapper { inner: Wrapper { inner: ctx.clone() } }.into_py(py));
        let _ = ideco.setattr("superclass", desc.superclass.as_str());
        let _ = ideco.setattr("child_types", desc.child_types.clone().into_py(py));
        let _ = ideco.setattr("children", PyChildrenWrapper { inner: Wrapper { inner: children.clone() } }.into_py(py));
        let _ = ideco.setattr("start", start.into_py(py));

        let matched = desc.inner.call_method0(py, "match")
            .unwrap()
            .downcast_bound::<PyBool>(py)
            .unwrap()
            .is_true();

        if !matched {
            return;
        }

        end = Some(ideco.getattr("end").unwrap().extract::<usize>().unwrap());

        // TODO: Actually merge intervals.
        for (visited_start, visited_end) in visited.iter() {
            let min = (*visited_start).max(start);
            let max = (*visited_end).min(end.unwrap());

            if max > min {
                println!("*** {} Matched invalidated region {}", node_type, &(*(*ctx).borrow().repr_ctx).borrow().printer.raw_text[min..max]);
                end = None;
                return;
            }
        }

        // if visited.contains(&(start, end.unwrap())) {
        //     end = None;
        //     return;
        // }

        let mut tree_children: HashMap<String, NodeIndex> = HashMap::default();
        let c: Ref<HashMap<String, TemplateChild>> = (*children).borrow();

        for (key, val) in c.iter() {
            match val {
                TemplateChild::Index(idx) => {
                    tree_children.insert(key.clone(), *idx);
                },
                TemplateChild::Seq(seq) => {
                    let seq_idx = make_seq_ref(pool.clone(), seq.to_vec());
                    tree_children.insert(key.clone(), seq_idx);
                }
            }
        }

        // Fill in any zero-length sequences.
        for (child_name, child_type) in &desc.child_types {
            if !c.contains_key(child_name) && child_type.starts_with("list") {
                let seq_idx = make_seq_ref(pool.clone(), vec![]);
                tree_children.insert(child_name.clone(), seq_idx);
            }
        }

        println!("Matched {} ({})", node_type, desc.superclass);
        let old_idxs = get_matched_idxs(start, end.unwrap(), desc.src_superclass.as_ref(), pool.clone(), ctx.clone(), lang.clone());

        if old_idxs.len() > 1 && desc.src_superclass.is_none() {
            let seq_idx = get_seq_idx(&old_idxs, pool.clone());
            visited.insert((start, end.unwrap()));
            make_new_seq(seq_idx, old_idxs, replacements, &node_type, tree_children, pool.clone(), ctx.clone(), lang.clone());
        } else if old_idxs.len() > 0 {
            if let Some(src_super) = desc.src_superclass.as_ref() {
                if (*pool).borrow().get(old_idxs[0]).descends_from(src_super) {
                    visited.insert((start, end.unwrap()));
                    make_new_tree(old_idxs[0], replacements, &node_type, tree_children, pool.clone(), ctx.clone(), lang.clone());
                } else {
                    println!("*** {:?} does not descend from {}", (*pool).borrow().get(old_idxs[0]).name, src_super);
                    end = None;
                }
            } else if desc.is_seq {
                let seq_idx = get_seq_idx(&old_idxs, pool.clone());
                visited.insert((start, end.unwrap()));
                insert_new_seq(seq_idx, old_idxs, replacements, &node_type, tree_children, pool.clone(), ctx.clone(), lang.clone());
                // println!("{}", (*pool).borrow().items[root].str(pool.clone()));
                // (*pool).borrow().items[root].print_tree(pool.clone(), 0);
            } else {
                println!("*** Rule {} has no src superclass", desc.name());
                end = None;
            }
        } else {
            end = None;
        }
    });

    end
}

#[allow(deprecated)]
pub fn lift_desc_literal(
    first_lit: &String,
    desc: &NodeDescriptor,
    node_type: String,
    root: NodeIndex,
    visited: &mut HashSet<(usize, usize)>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) -> bool {
    let mut replacements = Vec::new();

    let c = ctx.clone();
    let cb = (*c).borrow();
    let rc = cb.repr_ctx.clone();
    let rcb = (*rc).borrow();
    let printer = &rcb.printer;
    let text = &printer.raw_text;

    let mut start = 0;

    while let Some(off) = text[start..].find(first_lit) {
        if let Some(end) = lift_desc_at(
            start + off,
            desc,
            node_type.clone(),
            root,
            &mut replacements,
            visited,
            pool.clone(),
            ctx.clone(),
            lang.clone(),
        ) {
            start = end;
        } else {
            start += off + first_lit.len();
        }
    }

    replacements.len() > 0
}

#[allow(deprecated)]
pub fn lift_desc_node_type(
    first_type: &String,
    desc: &NodeDescriptor,
    node_type: String,
    root: NodeIndex,
    visited: &mut HashSet<(usize, usize)>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) -> bool {
    let mut replacements = Vec::new();

    let c = ctx.clone();
    let cb = (*c).borrow();
    let rc = cb.repr_ctx.clone();
    let rcb = (*rc).borrow();
    let printer = &rcb.printer;

    let starts = printer.node_starts.get(first_type).cloned().unwrap_or(vec![]);
    let mut idx = 0;

    while idx < starts.len() {
        if let Some(end) = lift_desc_at(
            starts[idx],
            desc,
            node_type.clone(),
            root,
            &mut replacements,
            visited,
            pool.clone(),
            ctx.clone(),
            lang.clone(),
        ) {
            while idx < starts.len() && starts[idx] < end {
                idx += 1;
            }
        } else {
            idx += 1;
        }
    }

    replacements.len() > 0
}

#[allow(deprecated)]
pub fn lift_desc(
    desc: &NodeDescriptor,
    node_type: String,
    root: NodeIndex,
    visited: &mut HashSet<(usize, usize)>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) -> bool {
    if !desc.has_template {
        return false;
    }

    match &desc.first_token {
        Token::Literal(first_type) => lift_desc_literal(first_type, desc, node_type, root, visited, pool, ctx, lang),
        Token::Type(first_type) => lift_desc_node_type(first_type, desc, node_type, root, visited, pool, ctx, lang),
    }

}

#[allow(deprecated)]
pub fn lift(
    root: NodeIndex,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    lang: Rc<RefCell<Language>>,
) -> bool {
    let mut changed = false;
    let modules = (*lang).borrow().modules.clone();
    let mut visited = HashSet::new();

    let mut all_descriptors = Vec::new();

    for (module_name, module) in modules.iter() {
        if module_name == "base" || module_name == "hlil" {
            continue;
        }

        let mut descriptors: Vec<(String, NodeDescriptor)> = module.descriptors.iter().map(|(n, d)| (format!("{}.{}", module_name, n), d.clone())).collect();
        all_descriptors.append(&mut descriptors);
    }

    all_descriptors.sort_by(|(_, d1), (_, d2)| d1.priority.cmp(&d2.priority));

    for (name, desc) in all_descriptors {
        // println!("  === Scanning for matches for {}", name);
        changed |= lift_desc(&desc, name, root, &mut visited, pool.clone(), ctx.clone(), lang.clone());
    }

    // HACK
    let n = (*pool).borrow().items.len();
    let mut num_uses = Vec::new();

    for idx in 0..n {
        (*pool).borrow_mut().get_mut(idx).parents.clear();
        num_uses.push(0);
    }

    let mut buf = vec![(root, None, false)];

    while let Some((idx, parent, is_lhs)) = buf.pop() {
        let is_hidden = (*pool).borrow().get(idx).is_hidden;

        if let Some(p) = parent {
            (*pool).borrow_mut().get_mut(idx).parents.push(p);

            if (
                !is_lhs &&
                (*pool).borrow().get(p).inputs().contains(&idx) &&
                !(*pool).borrow().get(p).descends_from("hlil.VarDecl")
            ) && !is_hidden {
                num_uses[idx] += 1;
            }
        }

        if !is_hidden {
            if let HlilKind::Seq(elems) = &(*pool).borrow().get(idx).kind {
                for elem in elems {
                    buf.push((*elem, Some(idx), false));
                }
            } else if let HlilKind::Tree(children) = &(*pool).borrow().get(idx).kind {
                for (name, child) in children {
                    // HACK: Actually encode the data flow in the descriptor.
                    buf.push((*child, Some(idx), name == "output"));
                }
            }
        }
    }

    for idx in 0..n {
        if (*pool).borrow().get(idx).name.as_ref() == Some(&"hlil.Var".to_string()) {
            // println!("{}: {}", (*pool).borrow().get(idx).str(pool.clone()), num_uses[idx]);

            if num_uses[idx] == 0 {
                let parents = (*pool).borrow().get(idx).parents.clone();

                for parent in parents {
                    if (*pool).borrow().get(parent).descends_from("hlil.VarDecl") ||
                        (*pool).borrow().get(parent).descends_from("hlil.Assign")
                    {
                        (*pool).borrow_mut().get_mut(parent).is_hidden = true;
                    }
                }
            }
        }

        // if num_uses[idx] == 0 && (*pool).borrow().get(idx).name.as_ref() == Some(&"hlil.Var".to_string()) {
        //     println!("{} is dead", (*pool).borrow().get(idx).str(pool.clone()));
        // }
    }

    changed
}

#[derive(serde::Deserialize, serde::Serialize, Clone)]
pub struct Function {
	pub addr: u64,
    pub root: NodeIndex,

	#[serde(serialize_with = "serialize_pool")]
	#[serde(deserialize_with = "deserialize_pool")]
    pub pool: Rc<RefCell<NodePool>>,

	#[serde(skip)]
    pub repr_ctx: Rc<RefCell<ReprContext>>,

	#[serde(serialize_with = "serialize_folds")]
	#[serde(deserialize_with = "deserialize_folds")]
	pub folded_exprs: Rc<RefCell<HashSet<NodeIndex>>>,

	pub var_map: HashMap<NodeIndex, NodeIndex>,

	pub xrefs: HashSet<(u64, u64)>,
}

impl Default for Function {
	fn default() -> Self {
		Function {
			addr: 0,
			root: 0,
			pool: Rc::new(RefCell::new(NodePool::new())),
			repr_ctx: Rc::new(RefCell::new(ReprContext::default())),
			folded_exprs: Rc::new(RefCell::new(HashSet::default())),
			var_map: HashMap::default(),
			xrefs: HashSet::default(),
		}
	}
}

pub fn get_repr_context(
    p: Rc<RefCell<NodePool>>,
    hlil: NodeIndex,
    lang: Rc<RefCell<Language>>,
) -> Rc<RefCell<ReprContext>> {
    let repr_ctx = ReprContext::new();
    let repr_c = Rc::new(RefCell::new(repr_ctx));

    {
        let pc = p.clone();
        let pb = (*pc).borrow();
        let node = pb.get(hlil);

        let _ = node.to_string(p.clone(), repr_c.clone(), Rc::new(RefCell::new(HashSet::default())));
        // node.print_tree(p.clone(), 0);
    }

    let tmpl_c = Rc::new(RefCell::new(TemplateContext { repr_ctx: repr_c.clone() }));

    let mut changed = lift(hlil, p.clone(), tmpl_c, lang.clone());
    // changed |= optimize(p.clone(), k

    while changed {
        let repr_ctx = ReprContext::new();
        let repr_c = Rc::new(RefCell::new(repr_ctx));

        {
            let pc = p.clone();
            let pb = (*pc).borrow();
            let node = pb.get(hlil);
            let _ = node.to_string(p.clone(), repr_c.clone(), Rc::new(RefCell::new(HashSet::default())));
            // println!("{}", s);
            // node.print_tree(p.clone(), 0);
            // log("LIFT", format!("After lifting function is:\n{}", s));
        }

        let tmpl_c = Rc::new(RefCell::new(TemplateContext { repr_ctx: repr_c.clone() }));
        changed = lift(hlil, p.clone(), tmpl_c, lang.clone());
    }

    log("LIFT", "set_finished".to_string());

    let repr_ctx = ReprContext::new();
    let repr_c = Rc::new(RefCell::new(repr_ctx));

    {
        let pc = p.clone();
        let pb = (*pc).borrow();
        let node = pb.get(hlil);
        let _ = node.to_string(p.clone(), repr_c.clone(), Rc::new(RefCell::new(HashSet::default())));
        // println!("{}", s);
    }

    repr_c
}

impl Function {
	pub fn new(
		addr: u64,
        func_json: &HashMap<String, JsonValue>,
		lang: Rc<RefCell<Language>>,
		params: Option<(Vec<NodeIndex>, Rc<RefCell<NodePool>>)>,
	) -> Self {
		let mut pool = NodePool::new();
		let mut var_map = HashMap::default();
		let xrefs = HashSet::default();

		// HACK: If we have parameters, create dummy "vars" regardless of their actual descriptor,
		//		 and register them as arg0, ..., argN in the node pool.
		if let Some((params, parent_pool)) = params {
			for (i, param_idx) in params.iter().enumerate() {
				let new_idx = deep_copy(&mut pool, *param_idx, parent_pool.clone(), &mut var_map, true);
				let arg_name = format!("arg{}", i + 1);
				pool.vars.insert(arg_name, new_idx);
			}
		}

        let body_json = &func_json["hlil"];
		let body = HlilNode::from(&body_json, &*lang, &mut pool, None).unwrap();

        let input_types_json: &Vec<_> = func_json["input_types"].get().unwrap();
		let mut inputs = vec![];

        for t in input_types_json {
            let t: &HashMap<_, _> = t.get().unwrap();
            let ops: &Vec<_> = t[&"operands".to_string()].get().unwrap();
            let name: &String = ops[0].get().unwrap();
            let dt: &String = ops[1].get().unwrap();
            let size: f64 = *ops[2].get().unwrap();

            let dt = make_data_type(&mut pool, dt.clone(), size as usize, &*lang);
            let dt = AttrVal::Node(dt);
            let var = make_var(&mut pool, name.clone(), dt, &*lang);

            let mut children = HashMap::new();
            children.insert("var".to_string(), var);
            let var_proto = make_tree(&mut pool, "hlil.VarProto", children, &*lang, 0);
            inputs.push(var_proto);
        }

        let j: &Vec<_> = func_json["return_type"].get().unwrap();
		let output_type: String = j[0].clone().try_into().unwrap();
        // let output_size: f64 = j[1].clone().try_into().unwrap();
        let name: String = func_json["name"].clone().try_into().unwrap();

		let mut children = HashMap::default();
		children.insert("name".to_string(), make_data(&mut pool, addr, name, "hlil.Symbol", &*lang));
		children.insert("body".to_string(), body);
		children.insert("inputs".to_string(), make_seq(&mut pool, inputs));
		// children.insert("output_type".to_string(), make_data_type(&mut pool, output_type, output_size as usize, &*lang));

		let hlil = make_tree(&mut pool, "hlil.Function", children, &*lang, addr);
        pool.set_type(hlil, &output_type, &*lang);

		let p = Rc::new(RefCell::new(pool));
        let repr_c = get_repr_context(p.clone(), hlil, lang.clone());

		Function {
			addr: addr,
			root: hlil,
			pool: p,
			repr_ctx: repr_c,
			folded_exprs: Rc::new(RefCell::new(HashSet::default())),
			var_map: var_map,
			xrefs: xrefs,
		}
	}

    pub fn spawn_child(&self, idx: NodeIndex, lang: Rc<RefCell<Language>>) -> Self {
		let p = Rc::new(RefCell::new(NodePool::new()));
        let mut var_map = HashMap::new();
        let hlil = deep_copy_ref(p.clone(), idx, self.pool.clone(), &mut var_map, false);
        let repr_c = get_repr_context(p.clone(), hlil, lang);

		Function {
			addr: self.addr,
			root: hlil,
			pool: p,
			repr_ctx: repr_c,
			folded_exprs: Rc::new(RefCell::new(HashSet::default())),
			var_map: var_map,
			xrefs: HashSet::default(),
		}
    }

    #[allow(dead_code)]
	pub fn is_folded(&self, idx: NodeIndex) -> bool {
		(*self.folded_exprs).borrow().contains(&idx)
	}

    #[allow(dead_code)]
	pub fn toggle_fold(&mut self, idx: NodeIndex) {
		if self.is_folded(idx) {
			(*self.folded_exprs).borrow_mut().remove(&idx);
		} else {
			(*self.folded_exprs).borrow_mut().insert(idx);
		}
	}

	pub fn num_lines(&self) -> usize {
		(*self.repr_ctx.clone()).borrow().printer.lines.len()
	}

	pub fn set_var_name(&self, idx: NodeIndex, new_name: &String) {
		let p = self.pool.clone();
		let mut pb = (*p).borrow_mut();
		let var = pb.get(idx);

		if let HlilKind::Var(_) = &var.kind {
			let new_var = HlilNode {
				idx: idx,
				kind: HlilKind::Var(new_name.clone()),
				addr: None,
				attrs: var.attrs.clone(),
				name: var.name.clone(),
				parents: vec![],
				descriptor: var.descriptor.clone(),
                desc_inst: var.desc_inst.clone(),
                is_hidden: var.is_hidden,
			};

			pb.items[idx] = new_var;
		}
	}

	pub fn rerender(&self) -> String {
		{
			let rc = self.repr_ctx.clone();
			let mut rcb = (*rc).borrow_mut();
			rcb.printer.reset();
		}

		let pc = self.pool.clone();
		let pb = (*pc).borrow();
		let node = pb.get(self.root);
		node.to_string(self.pool.clone(), self.repr_ctx.clone(), self.folded_exprs.clone())
	}

    pub fn raw_string(&self) -> String {
        let pc = self.pool.clone();
        let pb = (*pc).borrow();
        let node = pb.get(self.root);
        node.raw_str(self.pool.clone())
    }
}
