// use std::collections::HashMap;
// use std::rc::Rc;
// use std::cell::RefCell;

// use pyo3::prelude::*;
// use pyo3::types::PyList;

// use nom::branch::*;
// use nom::bytes::complete::*;
// use nom::character::complete::*;
// use nom::error::*;
// use nom::sequence::*;
// use nom::*;

// pub type Res<T, U> = IResult<T, U, Error<T>>;

// use crate::template::TemplateChild;
// use crate::repr::{ident, ReprContext};
// use crate::pool::{NodeIndex, NodePool};
// use crate::hlil::HlilNode;

// #[allow(dead_code)]
// #[derive(Debug, PartialEq, Eq, Clone)]
// pub enum ExprVal {
//     Int(i64),
// }

// #[allow(dead_code)]
// #[derive(Debug, PartialEq, Eq, Clone)]
// pub enum Expr {
//     Child(String),
//     Elem(Box<Expr>, usize),
//     Attr(Box<Expr>, String),
//     Node(NodeIndex),
// }

// fn child_expr(input: &str) -> Res<&str, Box<Expr>> {
//     preceded(char('$'), ident)(input)
//     .map(|(next, res)| {
//         (next, Box::new(Expr::Child(res.to_string())))
//     })
// }

// fn node_expr(input: &str) -> Res<&str, Box<Expr>> {
//     preceded(char('#'), digit1)(input)
//     .map(|(next, res)| {
//         let idx = res.parse::<usize>().unwrap();
//         (next, Box::new(Expr::Node(idx)))
//     })
// }

// fn attr_expr(input: &str) -> Res<&str, Box<Expr>> {
//     delimited (
//         tag("${"),
//         separated_pair(expr, char('.'), ident),
//         char('}'),
//     )(input)
//     .map(|(next, res)| {
//         let child = res.0;
//         let attr = res.1.to_string();
//         (next, Box::new(Expr::Attr(child, attr)))
//     })
// }

// fn elem_expr(input: &str) -> Res<&str, Box<Expr>> {
//     delimited (
//         tag("${"),
//         tuple((
//             expr,
//             delimited(char('['), digit1, char(']')),
//         )),
//         char('}'),
//     )(input)
//     .map(|(next, res)| {
//         let list = res.0;
//         let idx = res.1.parse::<usize>().unwrap();
//         (next, Box::new(Expr::Elem(list, idx)))
//     })
// }

// fn expr(input: &str) -> Res<&str, Box<Expr>> {
//     alt((
//         child_expr,
//         attr_expr,
//         elem_expr,
//         node_expr,
//     ))(input)
// }

// impl Expr {
//     pub fn from_str(input: &str) -> Expr {
//         *expr(input).unwrap().1
//     }
// }

// enum ExprResult {
//     Node(HlilNode),
//     Seq(Vec<HlilNode>),
//     String(String),
// }

// impl ExprResult {
//     #[allow(deprecated)]
//     fn convert_to_py(&self, py: Python, pool: Rc<RefCell<NodePool>>) -> PyResult<PyObject> {
//         match self {
//             ExprResult::Node(node) => Python::with_gil(|py| node.convert_to_py(py, pool.clone())),
//             ExprResult::Seq(seq) => {
// 				let list = PyList::new(py, seq.iter().map(|n| {
// 					n.convert_to_py(py, pool.clone()).unwrap()
// 				}).collect::<Vec<PyObject>>());
// 				Ok(list.into())
//             },
//             ExprResult::String(s) => Python::with_gil(|py| Ok(s.to_object(py)))
//         }
//     }
// }

// impl Expr {
//     pub fn evaluate_with_child(&self, idx: NodeIndex, pool: Rc<RefCell<NodePool>>) -> PyResult<PyObject> {
//         let p = pool.clone();
//         let pb = (*p).borrow();
//         let node = pb.get(idx);

//         match self {
//             Expr::Child(name) => {
//                 if name == "self" {
//                     Python::with_gil(|py| node.convert_to_py(py, pool.clone()))
//                 } else {
//                     todo!();
//                 }
//             },
//             _ => todo!()
//         }
//     }

//     fn _evaluate_with_repr(&self, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>) -> ExprResult {
//         match self {
//             Expr::Child(name) => {
//                 let p = pool.clone();
//                 let pb = (*p).borrow();

//                 let curr_idx = (*ctx.clone()).borrow().curr_node().unwrap();
//                 let curr_node = pb.get(curr_idx);

//                 if name == "self" {
//                     ExprResult::Node(curr_node.clone())
//                 } else {
//                     let child_idx = curr_node.get_child(name).unwrap();
//                     let child_node = pb.get(child_idx);
//                     ExprResult::Node(child_node.clone())
//                 }
//             },
//             Expr::Attr(expr, attr) => {
//                 let result = expr._evaluate_with_repr(pool.clone(), ctx.clone());

//                 if let ExprResult::Node(node) = result {
//                     if attr == "node_type" {
//                         let nt = node.name.clone().unwrap();
//                         ExprResult::String(nt)
//                     } else if attr == "data_type" {
//                         let nt = node.attrs.get("type").map(|s| s.to_string()).unwrap_or("".to_string());
//                         ExprResult::String(nt)
//                     } else {
//                         let child_idx = node.get_child(attr).unwrap();
//                         let child_node = (*pool).borrow().get(child_idx).clone();
//                         ExprResult::Node(child_node)
//                     }
//                 } else {
//                     panic!()
//                 }
//             }
//             _ => todo!(),
//         }
//     }

//     pub fn evaluate_with_repr(&self, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>) -> PyResult<PyObject> {
//         let result = self._evaluate_with_repr(pool.clone(), ctx.clone());
//         Python::with_gil(|py| result.convert_to_py(py, pool.clone()))
//     }

//     fn _evaluate_with_template(&self, pool: Rc<RefCell<NodePool>>, children: Rc<RefCell<HashMap<String, TemplateChild>>>) -> ExprResult {
//         match self {
//             Expr::Child(name) => {
//                 let c = children.clone();
//                 let cb = (*c).borrow();

//                 match cb.get(name) {
//                     Some(TemplateChild::Index(node_idx)) => {
//                         let node = (*pool).borrow().get(*node_idx).clone();
//                         ExprResult::Node(node)
//                     },
//                     Some(TemplateChild::Seq(idxs)) => {
//                         let mut nodes = vec![];
//                         for idx in idxs {
//                             let node = (*pool).borrow().get(*idx).clone();
//                             nodes.push(node);
//                         }
//                         ExprResult::Seq(nodes)
//                     }
//                     _ => panic!(),
//                 }
//             },
//             Expr::Attr(expr, attr) => {
//                 let result = expr._evaluate_with_template(pool.clone(), children.clone());

//                 if let ExprResult::Node(node) = result {
//                     if attr == "node_type" {
//                         let nt = node.name.clone().unwrap();
//                         ExprResult::String(nt)
//                     } else if attr == "data_type" {
//                         let nt = node.attrs.get("type").map(|s| s.to_string()).unwrap_or("".to_string());
//                         ExprResult::String(nt)
//                     } else {
//                         let child_idx = node.get_child(attr).unwrap();
//                         let child_node = (*pool).borrow().get(child_idx).clone();
//                         ExprResult::Node(child_node)
//                     }
//                 } else {
//                     panic!()
//                 }
//             }
//             _ => todo!(),
//         }
//     }

//     pub fn evaluate_with_template(&self, pool: Rc<RefCell<NodePool>>, children: Rc<RefCell<HashMap<String, TemplateChild>>>) -> PyResult<PyObject> {
//         let result = self._evaluate_with_template(pool.clone(), children.clone());
//         Python::with_gil(|py| result.convert_to_py(py, pool.clone()))
//     }
// }
