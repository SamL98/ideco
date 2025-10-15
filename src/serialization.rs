use serde::{Deserialize, Serializer, Deserializer};
use serde::de::{
    SeqAccess,
    Visitor,
};
use serde::ser::{SerializeSeq};
use serde;

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::marker::PhantomData;

use super::lang::Language;
use super::pool::{NodeIndex, NodePool};
use super::hlil::{HlilNode, HlilKind};

struct VecVisitor<T> {
	marker: PhantomData<fn() -> Vec<T>>
}

impl<T> VecVisitor<T> {
    fn new() -> Self {
        VecVisitor {
			marker: PhantomData
		}
    }
}

impl<'de, T: Deserialize<'de>> Visitor<'de> for VecVisitor<T> {
    type Value = Vec<T>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("vec")
    }

    fn visit_seq<S>(self, mut access: S) -> Result<Self::Value, S::Error> where S: SeqAccess<'de>,
    {
		let mut elems = vec![];

        while let Some(elem) = access.next_element()? {
			elems.push(elem);
        }

		Ok(elems)
    }
}

pub fn serialize_pool<S>(pool: &Rc<RefCell<NodePool>>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
	let p = pool.clone();
		let pb = (*p).borrow();

	let mut seq = serializer.serialize_seq(Some(pb.items.len()))?;
	for node in &pb.items {
		seq.serialize_element(node)?;
	}
	seq.end()
}

pub fn serialize_folds<S>(folded_exprs: &Rc<RefCell<HashSet<NodeIndex>>>, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
	let f = folded_exprs.clone();
	let fb = (*f).borrow();

	let mut seq = serializer.serialize_seq(Some(fb.len()))?;
	for idx in fb.iter() {
		seq.serialize_element(idx)?;
	}
	seq.end()
}

pub fn deserialize_pool<'de, D>(deserializer: D) -> Result<Rc<RefCell<NodePool>>, D::Error> where D: Deserializer<'de> {
	let nodes = Vec::<HlilNode>::deserialize(deserializer)?;
	let mut datas = HashMap::default();
	let mut vars = HashMap::default();

	for node in &nodes {
		match &node.kind {
			HlilKind::Var(name) => vars.insert(name.clone(), node.idx),
			HlilKind::Data((addr, _)) => datas.insert(*addr, node.idx),
			_ => None,
		};
	}

	let pool = NodePool {
		items: nodes,
		vars: vars,
		datas: datas,
	};

	Ok(Rc::new(RefCell::new(pool)))
}

pub fn deserialize_folds<'de, D>(deserializer: D) -> Result<Rc<RefCell<HashSet<NodeIndex>>>, D::Error> where D: Deserializer<'de> {
	let folds = HashSet::from_iter(deserializer.deserialize_seq(VecVisitor::new())?);
	Ok(Rc::new(RefCell::new(folds)))
}

pub fn default_language() -> Rc<RefCell<Language>> {
	let lang = Language::load(&[]);
	Rc::new(RefCell::new(lang))
}
