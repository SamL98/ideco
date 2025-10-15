use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::*;
use nom::multi::*;
use nom::sequence::*;
use nom::*;

pub type Res<T, U> = IResult<T, U, Error<T>>;

use pyo3::types::PyList;
use pyo3::prelude::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Debug};

// use crate::log::log;
use crate::repr::ReprContext;
use crate::pool::{NodeIndex, NodePool};

#[allow(dead_code)]
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum AttrVal {
    Num(i32),
    Str(String),
    Node(usize),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct AttrExpr {
    pub child: Box<PatternExpr>,
    pub attr: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SeqExpr {
    pub child: String,
    pub separator: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PatternExpr {
    Attr(AttrExpr),
    Seq(SeqExpr),
    LooseSeq(SeqExpr),
    Child(String),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TemplatePiece {
    Empty,
    Newline,
    This,
    Space(usize),
    Indent(usize),
    Until,
    Anchor,
    Literal(String),
    Child(PatternExpr),
}

pub struct TemplateContext {
	pub repr_ctx: Rc<RefCell<ReprContext>>,
}

impl Display for PatternExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternExpr::Attr(a) => write!(f, "{}.{}", &*a.child, a.attr),
            PatternExpr::LooseSeq(s) => {
                let sep = if s.separator == "\n" { "NEWLINE" } else { &s.separator };
                write!(f, "${{{}*~<{}>}}", s.child, sep)
            },
            PatternExpr::Seq(s) => {
                let sep = if s.separator == "\n" { "NEWLINE" } else { &s.separator };
                write!(f, "${{{}*<{}>}}", s.child, sep)
            },
            PatternExpr::Child(c) => write!(f, "${{{}}}", c),
        }
    }
}

impl Display for TemplatePiece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplatePiece::Empty => write!(f, "<EMPTY>"),
            TemplatePiece::Newline => write!(f, "<NEWLINE>"),
            TemplatePiece::This => write!(f, "$this"),
            TemplatePiece::Until => write!(f, "until"),
            TemplatePiece::Anchor => write!(f, "anchor"),
            TemplatePiece::Space(n) => write!(f, "{}", " ".repeat(*n)),
            TemplatePiece::Indent(n) => write!(f, "indent({})", n),
            TemplatePiece::Literal(l) => write!(f, "{}", l),
            TemplatePiece::Child(c) => write!(f, "{}", c),
        }
    }
}

pub fn ident(input: &str) -> Res<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn child_expr(input: &str) -> Res<&str, PatternExpr> {
    ident(input)
    .map(|(next, res)| {
        (next, PatternExpr::Child(res.to_string()))
    })
}

fn attr_expr(input: &str) -> Res<&str, PatternExpr> {
    separated_pair(
        alt((child_expr, attr_expr)),
        char('.'),
        ident
    )(input)
    .map(|(next, res)| {
        (next, PatternExpr::Attr(AttrExpr {
            child: Box::new(res.0),
            attr: res.1.to_string()
        }))
    })
}

fn loose_seq_expr(input: &str) -> Res<&str, PatternExpr> {
    tuple((
        terminated(ident, tag("*~")),
        preceded(
            char('<'),
            take_until(">")
        )
    ))(input)
    .map(|(next, res)| {
        (&next[1..], PatternExpr::LooseSeq(SeqExpr {
            child: res.0.to_string(),
            separator: if res.1 == "NEWLINE" {
                "\n".to_string()
            } else {
                res.1.to_string()
            },
        }))
    })
}

fn seq_expr(input: &str) -> Res<&str, PatternExpr> {
    tuple((
        terminated(ident, char('*')),
        preceded(
            char('<'),
            take_until(">")
        )
    ))(input)
    .map(|(next, res)| {
        (&next[1..], PatternExpr::Seq(SeqExpr {
            child: res.0.to_string(),
            separator: if res.1 == "NEWLINE" {
                "\n".to_string()
            } else {
                res.1.to_string()
            },
        }))
    })
}

fn _pattern_expr(input: &str) -> Res<&str, PatternExpr> {
    alt((loose_seq_expr, seq_expr, attr_expr, child_expr))(input)
}

fn pattern_expr(input: &str) -> Res<&str, PatternExpr> {
    delimited(
        char('{'),
        _pattern_expr,
        char('}')
    )(input)
    .map(|(next, res)| {
        (next, res)
    })
}

fn pattern_term(input: &str) -> Res<&str, PatternExpr> {
    preceded(
        char('$'),
        alt((child_expr, pattern_expr))
    )(input)
}

fn expr_template_token(input: &str) -> Res<&str, TemplatePiece> {
    pattern_term(input)
    .map(|(next, res)| {
        (next, TemplatePiece::Child(res))
    })
}

fn lit_template_token(input: &str) -> Res<&str, TemplatePiece> {
    let res = take_till1(|c| c == ' ' || c == '\n' || c == '$' || c == '`')(input)
    .map(|(next, res)| {
        let s = res.to_string();
        let s = s.replace("<dol>", "$");
        Ok((next, TemplatePiece::Literal(s)))
    });

    match res {
        Ok(inner_res) => inner_res,
        Err(err) => Err(err)
    }
}

fn space_template_token(input: &str) -> Res<&str, TemplatePiece> {
    many1_count(char(' '))(input)
    .map(|(next, res)| {
        (next, TemplatePiece::Space(res))
    })
}

fn indent_template_token(input: &str) -> Res<&str, TemplatePiece> {
    many0_count(char(' '))(input)
    .map(|(next, res)| {
        (next, TemplatePiece::Indent(res))
    })
}

fn until_template_token(input: &str) -> Res<&str, TemplatePiece> {
    many1_count(tag("..."))(input)
    .map(|(next, _)| {
        (next, TemplatePiece::Until)
    })
}

fn anchor_template_token(input: &str) -> Res<&str, TemplatePiece> {
    many1_count(tag("^"))(input)
    .map(|(next, _)| {
        (next, TemplatePiece::Anchor)
    })
}

#[allow(dead_code)]
fn newline_template_token(input: &str) -> Res<&str, TemplatePiece> {
    line_ending(input)
    .map(|(next, _)| {
        (next, TemplatePiece::Newline)
    })
}

fn this_template_token(input: &str) -> Res<&str, TemplatePiece> {
    tag("$self")(input)
    .map(|(next, _)| {
        (next, TemplatePiece::This)
    })
}

fn _template_token(input: &str) -> Res<&str, TemplatePiece> {
    alt((
        this_template_token,
        expr_template_token,
        space_template_token,
        until_template_token,
        anchor_template_token,
        lit_template_token
    ))(input)
}

fn template_token(input: &str) -> Res<&str, TemplatePiece> {
    _template_token(input)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Template {
	cmds: Vec<TemplatePiece>,
}

impl Template {
    pub fn from_str(input: &str) -> Template {
        let mut cmds = separated_list1(
            line_ending,
            pair(indent_template_token, many0(template_token))
        )(input)
        .map(|(_, res)| {
            let mut all_pieces = vec![];

            for (idx, line_pieces) in res.iter().enumerate() {
                if idx > 0 {
                    all_pieces.push(line_pieces.0.clone());
                }

                for piece in &line_pieces.1 {
                    all_pieces.push(piece.clone());
                }

                // ignore trailing newlines.
                if res.len() > 1 && idx == res.len() - 2 && res[idx+1].0 == TemplatePiece::Newline {
                    break;
                }

                // also ignore preceding newlines.
                // TODO: Fix this conditional.
                if (idx < res.len() - 1 && idx > 0) || (idx == 0 && res.len() == 2) {
                // if idx < res.len() - 1 && res.len() > 1 {
                    all_pieces.push(TemplatePiece::Newline);
                }
            }

            // println!("{:?}", all_pieces);

            all_pieces
        }).unwrap();

        if matches!(cmds[0], TemplatePiece::Indent(0)) {
            cmds.remove(0);
        }

		Template {
			cmds: cmds,
		}
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateChild {
	Index(NodeIndex),
	Seq(Vec<NodeIndex>),
}

#[allow(deprecated)]
impl TemplateChild {
	// fn new_seq() -> TemplateChild {
	// 	TemplateChild::Seq(Vec::new())
	// }

	// fn push(&mut self, idx: usize) {
	// 	if let TemplateChild::Seq(elems) = self {
	// 		elems.push(idx);
	// 	}
	// }

	pub fn convert_to_py(&self, py: Python, pool: Rc<RefCell<NodePool>>) -> PyResult<PyObject> {
		match self {
			TemplateChild::Index(idx) => {
				let p = pool.clone();
				let pb = (*p).borrow();
				let child = pb.get(*idx);
				child.convert_to_py(py, pool.clone())
			},
			TemplateChild::Seq(idxs) => {
				let list = PyList::new(py, idxs.iter().map(|i| {
					let p = pool.clone();
					let pb = (*p).borrow();
					let child = pb.get(*i);
					child.convert_to_py(py, pool.clone()).unwrap()
				}).collect::<Vec<PyObject>>());
				Ok(list.into())
			}
		}
	}
}

fn get_indent_level(text: &str, mut i: usize) -> (usize, usize) {
    let mut tmp_level = 0;
    let mut orig_indent_level = 0;
    let mut indent_level = 0;

    while i > 0 && &text[i..i+1] == " " {
        tmp_level += 1;
        i -= 1;
    }

    if i > 0 && &text[i..i+1] == "\n" {
        indent_level = tmp_level;
        orig_indent_level = tmp_level;
    }

    (indent_level, orig_indent_level)
}

pub fn evaluate_cmds(
    cmds: &[TemplatePiece],
    start: usize,
    children: Rc<RefCell<HashMap<String, TemplateChild>>>,
    pool: Rc<RefCell<NodePool>>,
    ctx: Rc<RefCell<TemplateContext>>,
    superclass: &str,
    child_types: &HashMap<String, String>,
    mut indent_level: usize,
    orig_indent_level: usize,
) -> Option<(usize, HashMap<String, TemplateChild>)> {
    use TemplatePiece::*;

    let mut bound_vars = HashMap::new();
    let mut cursor = start;

    if cmds.len() == 0 {
        return Some((start, bound_vars));
    }

    let c = ctx.clone();
    let cb = (*c).borrow();
    let rc = cb.repr_ctx.clone();
    let rcb = (*rc).borrow();

    let orig_text = &rcb.printer.raw_text;
    let node_ranges = &rcb.printer.raw_node_ranges;
    let text_ranges = &rcb.printer.raw_range_to_node;

    let text = &orig_text[cursor..];
    let mut num_cmds_processed = 1;
    // println!("=== {} \"{}\" ===", cmds[0], text[..text.len().min(20)].replace("\n", "\\n"));

    match &cmds[0] {
        Literal(_) | Space(_) | Newline | Indent(_) => {
            let lit = match &cmds[0] {
                Literal(lit) => lit.to_string(),
                Space(n) => " ".repeat(*n),
                Indent(level) => {
                    indent_level = orig_indent_level + *level;
                    // println!("Indent level is now {} ({} + {})", indent_level, orig_indent_level, *level);
                    " ".repeat(indent_level)
                },
                Newline => "\n".to_string(),
                _ => unreachable!(),
            };

            if &text[..lit.len().min(text.len())] == lit {
                cursor += lit.len();
            } else {
                // println!("Could not find literal \"{}\" at \"{}\"", lit, &text[..20.min(text.len())]);
                return None;
            }
        },
        Anchor => {
            // println!("De-indenting by {} ({})", indent_level, orig_indent_level);
            cursor -= indent_level;
        },
        Until => {
            todo!("until");
        },
        Child(PatternExpr::Child(child)) => {
            let child_superclass = &(*child_types)[child];
            let mut found = false;

            if let Some(ranges) = node_ranges.get(&cursor) {
                for (node_idx, end) in ranges {
                    if (*pool).borrow().get(*node_idx).descends_from(child_superclass) {
                        // println!("\"{}\" {} -- {} has type {}", &orig_text[cursor..*end], cursor, *end, child_superclass);
                        // println!("{}", orig_text);
                        if let Some((end, child_vars)) = evaluate_cmds(
                            &cmds[1..],
                            *end,
                            children.clone(),
                            pool.clone(),
                            ctx.clone(),
                            superclass,
                            child_types,
                            indent_level,
                            orig_indent_level
                        ) {
                            for (name, node) in child_vars {
                                bound_vars.insert(name, node);
                            }
                            bound_vars.insert(child.to_string(), TemplateChild::Index(*node_idx));
                            num_cmds_processed = cmds.len();
                            cursor = end;
                            found = true;
                            break;
                        }
                    }
                }
            }

            if !found {
                // println!("Could not find a {} at \"{}\"", child_superclass, &text[..20.min(text.len())]);
                return None;
            }
        },
        Child(PatternExpr::LooseSeq(SeqExpr { child, separator })) => {
            let separator = if separator == "\n" {
                format!("\n{}", " ".repeat(indent_level))
            } else {
                separator.clone()
            };
            let child_superclass = &(*child_types)[child];
            let child_superclass = &child_superclass[5..child_superclass.len() - 1];

            let leftover_superclass = &(*child_types)["leftovers"];
            let leftover_superclass = &leftover_superclass[5..leftover_superclass.len() - 1];

            let mut tmp_cursor = 0;
            let mut seq_idxs = Vec::new();
            let mut leftover_idxs = Vec::new();
            let mut ok = true;

            'outer: while let Some(off) = text[tmp_cursor..].find(&separator) {
                if cmds.len() > 1 {
                    match &cmds[1] {
                        Literal(_) | Space(_) | Newline | Indent(_) => {
                            let next_lit = match &cmds[1] {
                                Literal(lit) => lit.to_string(),
                                Space(n) => " ".repeat(*n),
                                Indent(level) => {
                                    let indent_level = orig_indent_level + *level;
                                    " ".repeat(indent_level)
                                },
                                Newline => "\n".to_string(),
                                _ => unreachable!(),
                            };

                            if &text[tmp_cursor..tmp_cursor + next_lit.len()] == next_lit {
                                break 'outer;
                            }
                        },
                        _ => todo!("{}", cmds[1]),
                    }
                }

                // println!("{}", &text[tmp_cursor..tmp_cursor + off]);
                if let Some(node_idxs) = text_ranges.get(&(cursor + tmp_cursor, cursor + tmp_cursor + off)) {
                    let mut found = false;

                    for node_idx in node_idxs {
                        // println!("  {:?} {}", (*pool).borrow().items[*node_idx].name, child_superclass);
                        if (*pool).borrow().get(*node_idx).descends_from(child_superclass) {
                            seq_idxs.push(*node_idx);
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        for node_idx in node_idxs {
                            // println!("  {:?} {}", (*pool).borrow().items[*node_idx].name, child_superclass);
                            if (*pool).borrow().get(*node_idx).descends_from(leftover_superclass) {
                                leftover_idxs.push(*node_idx);
                                found = true;
                                break;
                            }
                        }
                    }

                    if !found {
                        break;
                    }

                    tmp_cursor += off + separator.len();
                } else {
                    // println!("No nodes at {}", &text[..tmp_cursor + off]);
                    break;
                }
            }

            if cmds.len() > 1 {
                match &cmds[1] {
                    Literal(_) | Space(_) | Newline | Indent(_) => {
                        let next_lit = match &cmds[1] {
                            Literal(lit) => lit.to_string(),
                            Space(n) => " ".repeat(*n),
                            Indent(level) => {
                                let indent_level = orig_indent_level + *level;
                                " ".repeat(indent_level)
                            },
                            Newline => "\n".to_string(),
                            _ => unreachable!(),
                        };

                        if let Some(off) = text[tmp_cursor..].find(&next_lit) {
                            if let Some(node_idxs) = text_ranges.get(&(cursor + tmp_cursor, cursor + tmp_cursor + off)) {
                                let mut found = false;

                                for node_idx in node_idxs {
                                    if (*pool).borrow().get(*node_idx).descends_from(child_superclass) {
                                        seq_idxs.push(*node_idx);
                                        found = true;
                                        break;
                                    }
                                }

                                if !found {
                                    for node_idx in node_idxs {
                                        if (*pool).borrow().get(*node_idx).descends_from(leftover_superclass) {
                                            leftover_idxs.push(*node_idx);
                                            found = true;
                                            break;
                                        }
                                    }
                                }

                                if !found {
                                    ok = false;
                                }
                            }

                            tmp_cursor += off;
                        } else {
                            ok = false;
                        }
                    },
                    _ => todo!("{}", cmds[1]),
                };
            }

            if !ok {
                return None;
            }

            bound_vars.insert(child.to_string(), TemplateChild::Seq(seq_idxs));
            bound_vars.insert("leftovers".to_string(), TemplateChild::Seq(leftover_idxs));
            cursor += tmp_cursor;
        },
        Child(PatternExpr::Seq(SeqExpr { child, separator })) => {
            let child_superclass = &(*child_types)[child];
            let child_superclass = &child_superclass[5..child_superclass.len() - 1];

            let mut tmp_cursor = 0;
            let mut seq_idxs = Vec::new();
            let mut ok = true;

            'outer: while let Some(mut off) = text[tmp_cursor..].find(separator) {
                while text_ranges.get(&(cursor + tmp_cursor, cursor + tmp_cursor + off)).is_none() {
                    if let Some(sub_off) = text[tmp_cursor + off + separator.len()..].find(separator) {
                        off += separator.len() + sub_off;
                    } else {
                        break 'outer;
                    }
                }

                // println!("    Text: \"{}\"", &text[tmp_cursor..tmp_cursor + off]);

                if let Some(node_idxs) = text_ranges.get(&(cursor + tmp_cursor, cursor + tmp_cursor + off)) {
                    let mut found = false;

                    for node_idx in node_idxs {
                        // println!("  {:?} {}", (*pool).borrow().items[*node_idx].name, child_superclass);
                        if (*pool).borrow().get(*node_idx).descends_from(child_superclass) {
                            seq_idxs.push(*node_idx);
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        // println!("!!! Could not find {}", child_superclass);
                        break;
                    }

                    tmp_cursor += off + separator.len();

                    // let tot_indent = orig_indent_level + indent_level;
                    let tot_indent = indent_level;

                    if separator == "\n" && &text[tmp_cursor..text.len().min(tmp_cursor + tot_indent)] == &" ".repeat(tot_indent) {
                        tmp_cursor += tot_indent;
                    }
                } else {
                    break;
                }
            }

            // println!("==> Text After: \"{}\"", &text[tmp_cursor..tmp_cursor + (text.len() - tmp_cursor).min(20)]);

            if cmds.len() > 1 {
                if separator == "\n" {
                    tmp_cursor -= indent_level;
                }

                // println!("NEXT COMMAND: {}", cmds[1]);

                match &cmds[1] {
                    Literal(_) | Space(_) | Newline | Indent(_) => {
                        let next_lit = match &cmds[1] {
                            Literal(lit) => lit.to_string(),
                            Space(n) => " ".repeat(*n),
                            Indent(level) => {
                                let indent_level = orig_indent_level + *level;
                                " ".repeat(indent_level)
                            },
                            Newline => "\n".to_string(),
                            _ => unreachable!(),
                        };

                        if let Some(off) = text[tmp_cursor..].find(&next_lit) {
                            // println!("    Text: \"{}\"", &text[tmp_cursor..tmp_cursor + off]);

                            if let Some(node_idxs) = text_ranges.get(&(cursor + tmp_cursor, cursor + tmp_cursor + off)) {
                                let mut found = false;

                                for node_idx in node_idxs {
                                    if (*pool).borrow().get(*node_idx).descends_from(child_superclass) {
                                        seq_idxs.push(*node_idx);
                                        found = true;
                                        break;
                                    }
                                }

                                if !found {
                                    ok = false;
                                }
                            }

                            tmp_cursor += off;
                        } else {
                            ok = false;
                        }
                    },
                    _ => todo!("{}", cmds[1]),
                };
            }

            if !ok {
                return None;
            }

            bound_vars.insert(child.to_string(), TemplateChild::Seq(seq_idxs));
            cursor += tmp_cursor;
        },
        _ => todo!("{}", cmds[0]),
    };

    if let Some((end, child_vars)) = evaluate_cmds(
        &cmds[num_cmds_processed..],
        cursor,
        children,
        pool,
        ctx,
        superclass,
        child_types,
        indent_level,
        orig_indent_level
    ) {
        for (name, child) in child_vars {
            bound_vars.insert(name, child);
        }
        Some((end, bound_vars))
    } else {
        None
    }
}

impl Template {
	pub fn evaluate_with(
		&self,
		start: usize,
		children: Rc<RefCell<HashMap<String, TemplateChild>>>,
		pool: Rc<RefCell<NodePool>>,
		ctx: Rc<RefCell<TemplateContext>>,
		superclass: &str,
		child_types: &HashMap<String, String>,
	) -> Option<usize> {
		let c = ctx.clone();
		let cb = (*c).borrow();
		let rc = cb.repr_ctx.clone();
		let rcb = (*rc).borrow();

        let orig_text = &rcb.printer.raw_text;

        let i = start.saturating_sub(1);
        let (indent_level, orig_indent_level) = get_indent_level(&orig_text, i);

        if let Some((end, bound_vars)) = evaluate_cmds(
            &self.cmds,
            start,
            children.clone(),
            pool,
            ctx,
            superclass,
            child_types,
            indent_level,
            orig_indent_level
        ) {
            for (k, v) in &bound_vars {
                (*children).borrow_mut().insert(k.clone(), v.clone());
            }
            Some(end)
        } else {
            None
        }
	}
}
