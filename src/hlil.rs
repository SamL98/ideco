use tinyjson::JsonValue;
use pyo3::prelude::*;
use pyo3::types::{PyTuple, PyList, PyDict};

use std::collections::{HashSet, HashMap};
use std::fmt::{self, Display, Debug};
use std::cell::RefCell;
use std::rc::Rc;
use std::hash::{Hash, Hasher};

use crate::pool::*;
use crate::lang::*;
use crate::repr::{ReprContext, AttrVal, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum Opcode {
    Eq, Neq, And, Or, Xor, Add, Sub, Mul,
    Deref, AddrOf, SignExt, ZeroExt, BoolNot,
	Ult, Ule, Slt, Sle, Uge, Ugt, Sge, Sgt,
	Rol, Ror, Lsr, Asr, Lsl, Negate, Sbb,
    Divs, Divu, Mods, Modu, Split, Rlc, Rrc,
    Adc, Fadd, Fsub, Fmul, Fdiv, FcmpE, FcmpNe,
    FcmpLt, FcmpLe, FcmpGe, FcmpGt, FcmpUo, FcmpO,
    Fsqrt, Fneg, Fabs, FloatToInt, IntToFloat, RoundToInt, BoolToInt,
    Floor, Ceil, Ftrunc, Fconv, TestBit, AddOverflow,
	Intrinsic(String),
}

impl Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use Opcode::*;
        match self {
            Eq => write!(f, "=="),
            Neq => write!(f, "!="),
            Negate => write!(f, "-"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            BoolNot => write!(f, "!"),
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Xor => write!(f, "^"),
            AddrOf => write!(f, "&"),
            Deref => write!(f, "*"),
            SignExt => write!(f, "sx"),
            ZeroExt => write!(f, "zx"),
			Ult => write!(f, "<"),
			Ule => write!(f, "<="),
			Slt => write!(f, "<"),
			Sle => write!(f, "<="),
			Ugt => write!(f, ">"),
			Uge => write!(f, ">="),
			Sge => write!(f, ">="),
			Sgt => write!(f, ">"),
			Ror => write!(f, "ror"),
			Rol => write!(f, "rol"),
			Sbb => write!(f, "sbb"),
			Split => write!(f, "split"),
            Lsr => write!(f, "u>>"),
            Asr => write!(f, "s>>"),
            Lsl => write!(f, "<<"),
            Divs => write!(f, "s/"),
            Divu => write!(f, "u/"),
            Mods => write!(f, "s%"),
            Modu => write!(f, "u%"),
            Rlc => write!(f, "rlc"),
            Rrc => write!(f, "rrc"),
            Adc => write!(f, "adc"),
            Fadd => write!(f, "f+"),
            Fsub => write!(f, "f-"),
            Fmul => write!(f, "f*"),
            Fdiv => write!(f, "f/"),
            FcmpE => write!(f, "f=="),
            FcmpNe => write!(f, "f!="),
            FcmpLt => write!(f, "f<"),
            FcmpLe => write!(f, "f<="),
            FcmpGe => write!(f, "f>="),
            FcmpGt => write!(f, "f>"),
            FcmpO => write!(f, "both_nan"),
            FcmpUo => write!(f, "either_nan"),
            Fsqrt => write!(f, "fsqrt"),
            Fneg => write!(f, "f-"),
            Fabs => write!(f, "fabs"),
            FloatToInt => write!(f, "float2int"),
            IntToFloat => write!(f, "float.s"),
            RoundToInt => write!(f, "round2int"),
            BoolToInt => write!(f, "bool2int"),
            Floor => write!(f, "floor"),
            Ceil => write!(f, "ceil"),
            Ftrunc => write!(f, "ftrunc"),
            Fconv => write!(f, "fconv"),
            TestBit => write!(f, "test_bit"),
            AddOverflow => write!(f, "add_overflow"),
			Intrinsic(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum HlilKind {
    Int(u64),
    Float(f64),
    Opcode(Opcode),
    Str((String, u64)),
    Var(String),
    Data((u64, String)),
    Seq(Vec<NodeIndex>),
    Tree(HashMap<String, NodeIndex>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct HlilNode {
    pub idx: usize,
    pub kind: HlilKind,
    pub addr: Option<u64>,
    pub attrs: HashMap<String, AttrVal>,
	pub name: Option<String>,
    pub parents: Vec<NodeIndex>,
	#[serde(skip)]
    pub descriptor: Option<NodeDescriptor>,
	#[serde(skip)]
    pub desc_inst: Option<NodeInstance>,
    pub is_hidden: bool,
}

// For serde. blech
impl Default for HlilNode {
	fn default() -> Self {
		HlilNode {
			idx: 0,
			kind: HlilKind::Int(0),
            addr: None,
			attrs: HashMap::default(),
			name: None,
            parents: vec![],
            descriptor: None,
			desc_inst: None,
            is_hidden: false,
		}
	}
}

impl PartialEq for HlilNode {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for HlilNode {}

impl Debug for HlilKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use HlilKind::*;
        match self {
            Int(int) => write!(f, "Int(0x{:x})", int),
            Float(float) => write!(f, "Float({})", float),
            Str((_, addr)) => write!(f, "String(0x{:x})", addr),
            Var(name) => write!(f, "Var({})", name),
            Opcode(op) => write!(f, "Opcode({})", op),
            Data((addr, _)) => write!(f, "Data(0x{:x})", addr),
            Seq(elems) => write!(f, "Seq({:?})", elems),
            Tree(tree) => write!(f, "Tree({:?})", tree),
        }
    }
}

impl Hash for HlilKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use HlilKind::*;
        match self {
            Int(int) => int.hash(state),
            Float(float) => format!("{}", float).hash(state),
            Str((_, addr)) => addr.hash(state),
            Var(name) => name.hash(state),
            Opcode(op) => op.hash(state),
            Data((addr, _)) => addr.hash(state),
            Seq(elems) => {
                for e in elems {
                    e.hash(state);
                }
            },
            Tree(tree) => {
                for (k, v) in tree {
                    k.hash(state);
                    v.hash(state);
                }
            },
        }
    }
}

impl Hash for HlilNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialEq for HlilKind {
    fn eq(&self, other: &Self) -> bool {
        use HlilKind::*;
        match (self, other) {
            (Int(int1), Int(int2)) => int1 == int2,
            (Str((_, a1)), Str((_, a2))) => a1 == a2,
            (Var(n1), Var(n2)) => n1 == n2,
            (Opcode(op1), Opcode(op2)) => op1 == op2,
            (Data((a1, _)), Data((a2, _))) => a1 == a2,
            (Seq(e1), Seq(e2)) => e1.len() == e2.len() && (0..e1.len()).all(|i| e1[i] == e2[i]),
            (Tree(t1), Tree(t2)) => t1 == t2,
            _ => false,
        }
    }
}

impl Eq for HlilKind {}

unsafe impl Sync for HlilNode {}
unsafe impl Send for HlilNode {}

impl HlilNode {
    pub fn print_tree(&self, pool: Rc<RefCell<NodePool>>, indent_level: usize) {
        use HlilKind::*;

        if let Seq(elems) = &self.kind {
            println!("{}Seq", " ".repeat(2 * indent_level));

            for elem in elems {
                (*pool).borrow().get(*elem).print_tree(pool.clone(), indent_level + 1);
            }
        } else if let Tree(children) = &self.kind {
            println!("{}{}", " ".repeat(2 * indent_level), self.name.as_ref().unwrap());

            for (child_name, child) in children {
                println!("{}{}:", " ".repeat(2 * (indent_level + 1)), child_name);
                (*pool).borrow().get(*child).print_tree(pool.clone(), indent_level + 2);
            }
        } else if let Int(i) = &self.kind {
            println!("{}Int(0x{:x})", " ".repeat(2 * indent_level), i);
        } else if let Float(f) = &self.kind {
            println!("{}Float({})", " ".repeat(2 * indent_level), f);
        } else if let Str((s, _)) = &self.kind {
            println!("{}Str(\"{}\")", " ".repeat(2 * indent_level), s);
        } else if let Data((a, d)) = &self.kind {
            println!("{}Data(0x{:x}, {})", " ".repeat(2 * indent_level), a, d);
        } else if let Var(n) = &self.kind {
            println!("{}Var({})", " ".repeat(2 * indent_level), n);
        } else if let Opcode(o) = &self.kind {
            println!("{}Opcode({:?})", " ".repeat(2 * indent_level), o);
        }
    }

    pub fn find_all(&self, node_type: &String, pool: Rc<RefCell<NodePool>>, idxs: &mut Vec<NodeIndex>, visited: &mut HashSet<NodeIndex>) {
        use HlilKind::*;

        if visited.contains(&self.idx) {
            return;
        }

        visited.insert(self.idx);

        // if self.name.as_ref() == Some(node_type) {
        if self.descends_from(node_type) {
            idxs.push(self.idx);
        }

        if let Seq(elems) = &self.kind {
            for elem in elems {
                (*pool).borrow().get(*elem).find_all(node_type, pool.clone(), idxs, visited);
            }
        } else if let Tree(children) = &self.kind {
            for (_, child) in children {
                // println!("{:?}", (*pool).borrow().get(*child));
                (*pool).borrow().get(*child).find_all(node_type, pool.clone(), idxs, visited);
            }
        }
    }

    pub fn find(&self, pool: Rc<RefCell<NodePool>>, predicate: &dyn Fn(&HlilNode) -> bool) -> Option<NodeIndex> {
        use HlilKind::*;

        if predicate(self) {
            return Some(self.idx);
        }

        if let Seq(elems) = &self.kind {
            for elem in elems {
                if let Some(idx) = (*pool).borrow().get(*elem).find(pool.clone(), predicate) {
                    return Some(idx);
                }
            }
        } else if let Tree(children) = &self.kind {
            for (_, child) in children {
                // println!("{:?}", (*pool).borrow().get(*child));
                if let Some(idx) = (*pool).borrow().get(*child).find(pool.clone(), predicate) {
                    return Some(idx);
                }
            }
        }

        None
    }

    #[allow(deprecated)]
    pub fn convert_to_py(&self, py: Python, _pool: Rc<RefCell<NodePool>>) -> PyResult<PyObject> {
        use HlilKind::*;
        match &self.kind {
            Int(int) => Ok(int.to_object(py)),
            Float(float) => Ok(float.to_object(py)),
            Opcode(op) => Ok(format!("{}", op).to_object(py)),
            Str((s, _)) => Ok(s.to_object(py)),
            Var(v) => Ok(v.to_object(py)),
            Data((addr, sym)) => {
				let tuple = PyTuple::new(py, vec![
					addr.to_object(py),
					sym.replace("\n", "\\n").to_object(py)
				]);
				Ok(tuple.into())
			},
            Seq(elems) => {
                let list = PyList::new(py, elems.iter().map(|i| {
					*i
                }).collect::<Vec<NodeIndex>>());
                Ok(list.into())
            },
            Tree(tree) => {
                let dict = PyDict::new(py);
                for (key, idx) in tree {
					let _ = dict.set_item(key, *idx);
                }
                Ok(dict.into())
            }
        }
    }

    pub fn inputs(&self) -> HashSet<NodeIndex> {
        match &self.kind {
            HlilKind::Tree(tree) => {
                let mut inputs = HashSet::new();

                for (k, v) in tree {
                    if k != "output" {
                        inputs.insert(*v);
                    }
                }

                inputs
            },
            HlilKind::Seq(seq) => {
                let mut inputs = HashSet::new();

                for e in seq {
                    inputs.insert(*e);
                }

                inputs
            },
            _ => HashSet::new(),
        }
    }

    pub fn get_child(&self, name: &String) -> Option<NodeIndex> {
        match &self.kind {
            HlilKind::Tree(tree) => {
                tree.get(name).copied()
            },
            _ => None,
        }
    }

    pub fn get_attr(&self, name: &String) -> Option<AttrVal> {
        self.attrs.get(name).cloned()
    }

    pub fn to_string(&self, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>, folded_exprs: Rc<RefCell<HashSet<NodeIndex>>>) -> String {
        (*ctx.clone()).borrow_mut().start_node(self);
        use HlilKind::*;

		let result = if (*folded_exprs.clone()).borrow().contains(&self.idx) {
			let s = "...".to_string();
			(*ctx.clone()).borrow_mut().printer.put_token(Token {
				lit: s.clone(),
				expr: Some(self.idx),
				attrs: vec![],
			});
			s
		} else {
			match &self.kind {
				Int(int) => {
					let s = format!("0x{:x}", int);
                    let tok = Token {
                        lit: s.clone(),
                        expr: Some(self.idx),
                        attrs: vec![],
                    };
					// (*ctx.clone()).borrow_mut().printer.put_token_str(&s);
					(*ctx.clone()).borrow_mut().printer.put_token(tok);
					s
				},
				Float(float) => {
					let s = format!("{}", float);
                    let tok = Token {
                        lit: s.clone(),
                        expr: Some(self.idx),
                        attrs: vec![],
                    };
					// (*ctx.clone()).borrow_mut().printer.put_token_str(&s);
					(*ctx.clone()).borrow_mut().printer.put_token(tok);
					s
				},
				Str(_) => {
                    let inst = self.desc_inst.as_ref().unwrap();
					self.descriptor.as_ref().unwrap().evaluate_repr_with(inst, pool.clone(), ctx.clone(), folded_exprs.clone())
				}
				Var(_) => {
                    let inst = self.desc_inst.as_ref().unwrap();
					self.descriptor.as_ref().unwrap().evaluate_repr_with(inst, pool.clone(), ctx.clone(), folded_exprs.clone())
				},
				Opcode(op) => {
					let s = format!("{}", op);
					(*ctx.clone()).borrow_mut().printer.put_token_str(&s);
					s
				}
				Data((_, s)) => {
					if let Some(desc) = self.descriptor.as_ref() {
                        let inst = self.desc_inst.as_ref().unwrap();
						desc.evaluate_repr_with(inst, pool.clone(), ctx.clone(), folded_exprs.clone())
					} else {
						let token = Token {
							lit: s.clone(),
							expr: Some(self.idx),
							attrs: vec![],
						};
						(*ctx.clone()).borrow_mut().printer.put_token(token);
						s.clone()
					}
				}
				Seq(elems) => {
					format!("[{}]", elems.iter().map(|e| (*pool).borrow().get(*e).to_string(pool.clone(), ctx.clone(), folded_exprs.clone())).collect::<Vec<String>>().join(", "))
				},
				Tree(_) => {
                    let inst = self.desc_inst.as_ref().unwrap();
					self.descriptor.as_ref().unwrap().evaluate_repr_with(inst, pool.clone(), ctx.clone(), folded_exprs.clone())
				},
			}
		};

        (*ctx).borrow_mut().end_node();
        result
    }

    pub fn raw_str(&self, pool: Rc<RefCell<NodePool>>) -> String {
        let ctx = Rc::new(RefCell::new(ReprContext::new()));
        let _ = self.to_string(pool, ctx.clone(), Rc::new(RefCell::new(HashSet::default())));
        let c = ctx.clone();
        let cb = (*c).borrow();
        cb.printer.raw_text.clone()
    }

    pub fn str(&self, pool: Rc<RefCell<NodePool>>) -> String {
        // let py = pool.py();
        // let mut ctx = Py::new(py, ReprContext::new()).unwrap().extract::<PyRefMut<ReprContext>>(py).unwrap();
        let ctx = Rc::new(RefCell::new(ReprContext::new()));
        self.to_string(pool, ctx, Rc::new(RefCell::new(HashSet::default())))
        // self.descriptor.as_ref().unwrap().evaluate_repr_with(pool, ctx)
    }

	pub fn descends_from(&self, base_type: &str) -> bool {
		self.descriptor.as_ref().map(|d| {
			d.descends_from(base_type)
		}).unwrap_or(match &self.kind {
			HlilKind::Int(_) => base_type == "base.Expr" || base_type == "list[base.Expr]",
			HlilKind::Data(_) => base_type == "base.Expr" || base_type == "list[base.Expr]",
			_ => false,
		})
	}

	pub fn references(&self, pool: Rc<RefCell<NodePool>>) -> Vec<u64> {
		self.descriptor.as_ref().map(|d| {
			d.references(self.idx, pool)
		}).unwrap_or(match &self.kind {
			// HlilKind::Int(int) => ,
			HlilKind::Data((addr, ..)) => vec![*addr],
			_ => vec![],
		})
	}
}

impl HlilNode {
    pub fn from_list(
        node: &Vec<JsonValue>,
		lang: &RefCell<Language>,
		pool: &mut NodePool,
		parent: Option<String>,
	) -> Option<NodeIndex> {
        let mut exprs = vec![];
        for e in node {
            if let Some(expr) = HlilNode::from(e, lang, pool, parent.clone()) {
                exprs.push(expr);
            }
        }
        Some(make_seq(pool, exprs))
    }

    pub fn from_dict(
        node: &HashMap<String, JsonValue>,
		lang: &RefCell<Language>,
		pool: &mut NodePool,
		parent: Option<String>,
	) -> Option<NodeIndex> {
        let kind: String = node[&"name".to_string()].clone().try_into().unwrap();
        let kind = kind.as_str();

        let op_list: Vec<_> = node[&"operands".to_string()].clone().try_into().unwrap();
        let operands = op_list.iter().map(|o| {
            HlilNode::from(o, lang, pool, Some(kind.to_string())).unwrap()
        }).collect::<Vec<NodeIndex>>();

        let addr: f64 = node.get(&"address".to_string()).map(|a| a.clone().try_into().unwrap()).unwrap_or(0.0);
        let addr = addr as u64;

        match kind {
            "Nop" => {
                let children = HashMap::default();
                Some(make_tree(pool, "hlil.Nop", children, lang, addr))
            },
            "Break" => {
                let children = HashMap::default();
                Some(make_tree(pool, "hlil.Break", children, lang, addr))
            },
            "Continue" => {
                let children = HashMap::default();
                Some(make_tree(pool, "hlil.Continue", children, lang, addr))
            },
            "Noret" => {
                let children = HashMap::default();
                Some(make_tree(pool, "hlil.NoReturn", children, lang, addr))
            },
            "Undef" | "Unimpl" | "Unreachable" => {
                let children = HashMap::default();
                Some(make_tree(pool, "hlil.Nop", children, lang, addr))
            },
			"Rol" | "Ror" | "Sbb" | "Split" |
            "Rlc" | "Rrc" | "Adc" | "TestBit" |
            "AddOverflow"=> {
                let op = match kind {
                    "Rol" => Opcode::Rol,
                    "Ror" => Opcode::Ror,
                    "Rlc" => Opcode::Rlc,
                    "Rrc" => Opcode::Rrc,
                    "Sbb" => Opcode::Sbb,
                    "Adc" => Opcode::Adc,
                    "Split" => Opcode::Split,
                    "TestBit" => Opcode::TestBit,
                    "AddOverflow" => Opcode::AddOverflow,
					_ => panic!(),
				};

                let mut children = HashMap::default();
                children.insert("lhs".to_string(), operands[0]);
                children.insert("rhs".to_string(), operands[1]);
                children.insert("op".to_string(), make_opcode(pool, op));
                Some(make_tree(pool, "hlil.BinaryFunc", children, lang, addr))
			}
            "Add" | "Sub" | "Mul" | "Xor" | "Or" | "And" | "CmpE" | "CmpNe"
                | "CmpUle" | "CmpUlt" | "CmpSlt" | "CmpSle"
                | "CmpUge" | "CmpUgt" | "CmpSgt" | "CmpSge" | "MulsDp" | "MuluDp"
                | "Lsr" | "Lsl" | "Asr" | "Divs" | "Divu" | "DivsDp" | "DivuDp"
                | "Mods" | "Modu" | "Fadd" | "Fsub" | "ModsDp" | "ModuDp"
                | "Fmul" | "Fdiv" | "FcmpE" | "FcmpNe" | "FcmpO" | "FcmpUo"
                | "FcmpLt" | "FcmpLe" | "FcmpGe" | "FcmpGt" => {
                let op = match kind {
                    "Add" => Opcode::Add,
                    "Sub" => Opcode::Sub,
                    "Mul" => Opcode::Mul,
                    "MulsDp" => Opcode::Mul,
                    "MuluDp" => Opcode::Mul,
                    "Xor" => Opcode::Xor,
                    "Or" => Opcode::Or,
                    "And" => Opcode::And,
                    "Divs" => Opcode::Divs,
                    "Divu" => Opcode::Divu,
                    "DivsDp" => Opcode::Divs,
                    "DivuDp" => Opcode::Divu,
                    "Mods" => Opcode::Mods,
                    "Modu" => Opcode::Modu,
                    "ModsDp" => Opcode::Mods,
                    "ModuDp" => Opcode::Modu,
                    "CmpE" => Opcode::Eq,
                    "CmpNe" => Opcode::Neq,
					"CmpUle" => Opcode::Ule,
					"CmpUlt" => Opcode::Ult,
					"CmpSle" => Opcode::Sle,
					"CmpSlt" => Opcode::Slt,
					"CmpUge" => Opcode::Uge,
					"CmpUgt" => Opcode::Ugt,
					"CmpSge" => Opcode::Sge,
					"CmpSgt" => Opcode::Sgt,
                    "Lsr" => Opcode::Lsr,
                    "Asr" => Opcode::Asr,
                    "Lsl" => Opcode::Lsl,
                    "Fadd" => Opcode::Fadd,
                    "Fsub" => Opcode::Fsub,
                    "Fmul" => Opcode::Fmul,
                    "Fdiv" => Opcode::Fdiv,
                    "FcmpE" => Opcode::FcmpE,
                    "FcmpNe" => Opcode::FcmpNe,
                    "FcmpLt" => Opcode::FcmpLt,
                    "FcmpLe" => Opcode::FcmpLe,
                    "FcmpGe" => Opcode::FcmpGe,
                    "FcmpGt" => Opcode::FcmpGt,
                    "FcmpO" => Opcode::FcmpO,
                    "FcmpUo" => Opcode::FcmpUo,
                    _ => panic!(),
                };

                let mut children = HashMap::default();
                children.insert("lhs".to_string(), operands[0]);
                children.insert("rhs".to_string(), operands[1]);
                children.insert("op".to_string(), make_opcode(pool, op));
                Some(make_tree(pool, "hlil.Binary", children, lang, addr))
            },
            "ArrayIndex" => {
                let mut children = HashMap::default();
                children.insert("array".to_string(), operands[0]);
                children.insert("index".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.ArrayAccess", children, lang, addr))
            },
            "ArrayIndexSsa" => todo!("ArrayIndexSsa"),
            "Assign" => {
                let mut children = HashMap::default();
                children.insert("output".to_string(), operands[0]);
                children.insert("rhs".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.Assign", children, lang, addr))
            },
            "AssignMemSsa" => todo!("AssignMemSsa"),
            "AssignUnpack" => {
                let mut children = HashMap::default();
                children.insert("exprs".to_string(), operands[0]);
                let lhs_idx = make_tree(pool, "hlil.ExprList", children, lang, addr);

                let mut children = HashMap::default();
                children.insert("output".to_string(), lhs_idx);
                children.insert("rhs".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.Assign", children, lang, addr))
            },
            "AssignUnpackMemSsa" => todo!("AssignUnpackMemSsa"),
            "Block" => {
                let mut children = HashMap::default();
                children.insert("stmts".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.Block", children, lang, addr))
            },
            "Call" => {
                let mut children = HashMap::default();
                children.insert("target".to_string(), operands[0]);
                children.insert("params".to_string(), operands[1]);
                let mut tree = make_tree(pool, "hlil.Call", children, lang, addr);

                // TODO: Add more types here.
                if parent.map(|p| matches!(p.as_str(), "Block" | "If" | "DoWhile")).unwrap_or(false) {
                    let mut children = HashMap::default();
                    children.insert("call".to_string(), tree);
                    tree = make_tree(pool, "hlil.CallStmt", children, lang, addr);
                }

                Some(tree)
            },
            "Tailcall" => {
                let mut children = HashMap::default();
                children.insert("target".to_string(), operands[0]);
                children.insert("params".to_string(), operands[1]);
                let call = make_tree(pool, "hlil.Call", children, lang, addr);

                let retvals = make_seq(pool, vec![call]);

                let mut children = HashMap::default();
                children.insert("retvals".to_string(), retvals);
                Some(make_tree(pool, "hlil.Return", children, lang, addr))
            },
            "CallSsa" => todo!("CallSsa"),
            "Case" => {
                let mut children = HashMap::default();
                children.insert("labels".to_string(), operands[0]);
                children.insert("body".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.Case", children, lang, addr))
            },
            "Const" => {
                Some(operands[0])
            },
            "Symbol" => {
                let addr: f64 = op_list[1].clone().try_into().unwrap();
                let addr = addr as u64;
                let name: String = op_list[0].clone().try_into().unwrap();

                if name.starts_with('"') {
                    Some(make_str(pool, name[1..name.len() - 1].to_string(), 0, lang))
                } else {
                    Some(make_data(pool, addr, name, "hlil.Symbol", lang))
                }
            },
            "ConstPtr" => {
                let addr: f64 = op_list[1].clone().try_into().unwrap();
                let addr = addr as u64;
                let name: String = op_list[0].clone().try_into().unwrap();
                Some(make_data(pool, addr, name, "hlil.Data", lang))
            },
            "Import" => {
                let addr: f64 = op_list[1].clone().try_into().unwrap();
                let addr = addr as u64;
                let name: String = op_list[0].clone().try_into().unwrap();
                Some(make_data(pool, addr, name, "hlil.Symbol", lang))
            },
            "ConstData" => {
                Some(operands[0])
            },
            "Deref" => {
                let mut children = HashMap::default();
                children.insert("operand".to_string(), operands[0]);
                // children.insert("op".to_string(), make_opcode(pool, Opcode::Deref));
                Some(make_tree(pool, "hlil.Deref", children, lang, addr))
            },
            "AddressOf" | "Neg" => {
                let opcode = match kind {
                    "AddressOf" => Opcode::AddrOf,
                    "Neg" => Opcode::Negate,
                    _ => panic!(),
                };

                let mut children = HashMap::default();
                children.insert("operand".to_string(), operands[0]);
                children.insert("op".to_string(), make_opcode(pool, opcode));
                Some(make_tree(pool, "hlil.Unary", children, lang, addr))
            },
            "Sx" | "Zx" | "Not" |
            "Fsqrt" | "Fneg" | "Fabs" |
            "FloatToInt" | "IntToFloat" | "RoundToInt" | "BoolToInt" |
            "Floor" | "Ceil" | "Ftrunc" | "FloatConv" => {
                let opcode = match kind {
                    "Sx" => Opcode::SignExt,
                    "Zx" => Opcode::ZeroExt,
                    "Not" => Opcode::BoolNot,
                    "Fsqrt" => Opcode::Fsqrt,
                    "Fneg" => Opcode::Fneg,
                    "Fabs" => Opcode::Fabs,
                    "FloatToInt" => Opcode::FloatToInt,
                    "IntToFloat" => Opcode::IntToFloat,
                    "RoundToInt" => Opcode::RoundToInt,
                    "BoolToInt" => Opcode::BoolToInt,
                    "Floor" => Opcode::Floor,
                    "Ceil" => Opcode::Ceil,
                    "Ftrunc" => Opcode::Ftrunc,
                    "FloatConv" => Opcode::Fconv,
                    _ => panic!(),
                };

                let mut children = HashMap::default();
                children.insert("operand".to_string(), operands[0]);
                children.insert("op".to_string(), make_opcode(pool, opcode));
                Some(make_tree(pool, "hlil.UnaryFunc", children, lang, addr))
            },
            "LowPart" => {
                let mut children = HashMap::default();
                children.insert("src".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.LowPart", children, lang, addr))
            },
            "UnimplMem" => todo!("UnimplMem"),
            "DerefFieldSsa" => todo!("DerefFieldSsa"),
            "DerefSsa" => todo!("DerefSsa"),
            "ExternPtr" => todo!("ExternPtr"),
            "FloatConst" => {
                let float: f64 = op_list[0].clone().try_into().unwrap();
                Some(make_float(pool, float))
            },
            "ForSsa" => todo!("ForSsa"),
            "Jump" => {
                let mut children = HashMap::default();
                children.insert("dest".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.Jump", children, lang, addr))
            },
            "Goto" => {
                let mut children = HashMap::default();
                children.insert("label".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.Goto", children, lang, addr))
            },
            "Label" => {
                let mut children = HashMap::default();
                children.insert("label".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.LabelDecl", children, lang, addr))
            },
            "If" => {
                let mut children = HashMap::default();
                children.insert("condition".to_string(), operands[0]);

                let mut node_name = "hlil.If";
                let mut body_name = "body";

                if operands.len() > 2 && pool.get(operands[2]).name != Some("hlil.Nop".to_string()) {
                    children.insert("else_body".to_string(), operands[2]);
                    node_name = "hlil.IfElse";
                    body_name = "if_body";
                }

                let mut if_body = operands[1];

                if !pool.get(operands[1]).descriptor.as_ref().unwrap().name().ends_with("Block") {
                    let seq_idx = make_seq(pool, vec![if_body]);

                    let mut new_children = HashMap::default();
                    new_children.insert("stmts".to_string(), seq_idx);

                    if_body = make_tree(pool, "hlil.Block", new_children, lang, addr);
                }

                children.insert(body_name.to_string(), if_body);
                Some(make_tree(pool, node_name, children, lang, addr))
            },
            "Intrinsic" => {
                let mut children = HashMap::default();
                let intrinsic: String = op_list[0].clone().try_into().unwrap();
                let op = make_opcode(pool, Opcode::Intrinsic(intrinsic));
                children.insert("op".to_string(), op);
                children.insert("params".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.Intrinsic", children, lang, addr))
			},
            "IntrinsicSsa" => todo!("IntrinsicSsa"),
            "MemPhi" => todo!("MemPhi"),
            "Ret" => {
                let mut children = HashMap::default();
                children.insert("retvals".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.Return", children, lang, addr))
            },
            "StructField" => {
                let mut children = HashMap::default();
                children.insert("struct".to_string(), operands[0]);
                children.insert("field".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.StructAccess", children, lang, addr))
            },
            "DerefField" => {
                let mut children = HashMap::default();
                children.insert("struct".to_string(), operands[0]);
                children.insert("field".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.StructFieldDeref", children, lang, addr))
            },
            "Switch" => {
                let mut children = HashMap::default();
                children.insert("condition".to_string(), operands[0]);
                children.insert("cases".to_string(), operands[2]);
                Some(make_tree(pool, "hlil.Switch", children, lang, addr))
            },
            "Syscall" => todo!("Syscall"),
            "SyscallSsa" => todo!("SyscallSsa"),
            "Trap" | "Bp" => {
                let mut children = HashMap::default();
                children.insert("num".to_string(), if kind == "Trap" {
                    operands[0]
                } else {
                    make_int(pool, 0x1337)
                });
                Some(make_tree(pool, "hlil.Trap", children, lang, addr))
            },
            "VarDeclare" => {
                let mut children = HashMap::default();
                children.insert("var".to_string(), operands[0]);
                Some(make_tree(pool, "hlil.VarDecl", children, lang, addr))
            },
            "Var" => {
                let name: String = op_list[0].clone().try_into().unwrap();
                let dt: String = op_list[1].clone().try_into().unwrap();
                let size: f64 = op_list[2].clone().try_into().unwrap();
                let dt = make_data_type(pool, dt, size as usize, lang);
                let dt = AttrVal::Node(dt);
                Some(make_var(pool, name, dt, lang))
            },
            "VarInit" => {
                let mut children = HashMap::default();
                children.insert("output".to_string(), operands[0]);
                children.insert("rhs".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.VarInit", children, lang, addr))
            },
            "VarInitSsa" => todo!("VarInitSsa"),
            "VarPhi" => todo!("VarPhi"),
            "VarSsa" => todo!("VarSsa"),
            "While" => {
                let mut children = HashMap::default();
                children.insert("condition".to_string(), operands[0]);
                children.insert("body".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.While", children, lang, addr))
			},
            "For" => {
                let mut children = HashMap::default();
                children.insert("init".to_string(), operands[0]);
                children.insert("condition".to_string(), operands[1]);
                children.insert("modify".to_string(), operands[2]);
                children.insert("body".to_string(), operands[3]);
                Some(make_tree(pool, "hlil.For", children, lang, addr))
			},
            "DoWhile" => {
                let mut children = HashMap::default();
                children.insert("condition".to_string(), operands[0]);
                children.insert("body".to_string(), operands[1]);
                Some(make_tree(pool, "hlil.DoWhile", children, lang, addr))
            },
            "WhileSsa" => todo!("WhileSsa"),
            "DoWhileSsa" => todo!("DoWhileSsa"),
            _ => todo!("{}", kind),
        }
    }

    pub fn from(
        node: &JsonValue,
		lang: &RefCell<Language>,
		pool: &mut NodePool,
		parent: Option<String>,
	) -> Option<NodeIndex> {
        match node {
            JsonValue::Array(list) => HlilNode::from_list(list, lang, pool, parent),
            JsonValue::Object(dict) => HlilNode::from_dict(dict, lang, pool, parent),
            JsonValue::Number(int) => Some(make_int(pool, (*int as i64) as u64)),
            JsonValue::String(s) => Some(make_str(pool, s.clone(), 0, lang)),
            _ => panic!(),
        }
    }
}
