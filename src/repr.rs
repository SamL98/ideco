use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Debug};
use std::collections::{HashSet, HashMap};

use pyo3::prelude::*;
use pyo3::types::{PyString, PyList};

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::*;
use nom::multi::*;
use nom::sequence::*;
use nom::*;

pub type Res<T, U> = IResult<T, U, Error<T>>;

use crate::hlil::{HlilKind, HlilNode};
use crate::pool::*;

#[derive(PartialEq, Eq, Clone, Debug, serde::Serialize, serde::Deserialize)]
pub enum AttrVal {
    Num(i64),
    Str(String),
    Node(usize),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct AttrExpr {
    pub child: Box<PatternExpr>,
    pub attr: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ElemExpr {
    pub child: Box<PatternExpr>,
    pub idx: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SeqExpr {
    pub child: String,
    pub separator: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PatternExpr {
    Attr(AttrExpr),
    Elem(ElemExpr),
    Seq(SeqExpr),
    Child(String),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ReprPiece {
    Empty,
    Newline,
    This,
    Space(usize),
    Indent(usize),
    Literal(String),
    Child(PatternExpr),
    Node(NodeIndex),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ReprAttr {
    Green, Blue, Red, Orange, Yellow, Purple, Underlined
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Token {
    pub lit: String,
    pub expr: Option<NodeIndex>,
    pub attrs: Vec<ReprAttr>,
}

impl Display for AttrVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrVal::Num(num) => write!(f, "0x{:x}", num),
            AttrVal::Str(s) => write!(f, "{}", s),
            _ => panic!(),
        }
    }
}

impl Display for ReprPiece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReprPiece::Empty => write!(f, "<EMPTY>"),
            ReprPiece::Newline => write!(f, "<NEWLINE>"),
            ReprPiece::This => write!(f, "$this"),
            ReprPiece::Space(n) => write!(f, "{}", " ".repeat(*n)),
            ReprPiece::Indent(n) => write!(f, "indent({})", n),
            ReprPiece::Literal(l) => write!(f, "{}", l),
            ReprPiece::Child(c) => write!(f, "{}", c),
            ReprPiece::Node(i) => write!(f, "#{}", i),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Repr {
    pub pieces: Vec<(ReprPiece, Vec<ReprAttr>)>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Printer {
    pub lines: Vec<Vec<Token>>,
    pub row: usize,
    pub col: usize,

    // Maps nodes to {row: (start_col, end-col)}.
    pub node_ranges: HashMap<NodeIndex, HashMap<usize, (usize, usize)>>,

    // At each index in the raw text (without escape codes), returns a vector
    // of each node that starts there and how long it.
    // TODO: See if I can get rid of this since nodes now have parents.
	pub raw_node_ranges: HashMap<usize, Vec<(NodeIndex, usize)>>,

    // Map a (start, end) index in the raw text to the vector of nodes there.
    pub raw_range_to_node: HashMap<(usize, usize), Vec<NodeIndex>>,

    // The reverse of raw_range_to_node.
    pub node_to_raw_range: HashMap<NodeIndex, (usize, usize)>,

    // Maps each row to (token, start_col, end_col).
    pub token_ranges: HashMap<usize, Vec<(Token, usize, usize)>>,

    // Maps (row, col) to hierarchy of nodes at that location.
    pub idx_stacks: HashMap<(usize, usize), Vec<NodeIndex>>,

    pub node_starts: HashMap<String, Vec<usize>>,

    pub attrs: HashMap<(usize, usize), Vec<ReprAttr>>,
    pub node_stack: Vec<(NodeIndex, usize, usize, usize)>,
    pub indent_level: usize,
    pub full_text: String,
	pub raw_text: String,
}

#[pyclass]
pub struct ReprContext {
    pub printer: Printer,
    pub indent_stack: Vec<(usize, usize)>,
    pub last_indent_idx: usize,
    pub indent: usize,
}

unsafe impl Sync for ReprContext {}
unsafe impl Send for ReprContext {}

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
        alt((child_expr, attr_expr, elem_expr)),
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

fn elem_expr(input: &str) -> Res<&str, PatternExpr> {
    tuple((
        alt((child_expr, attr_expr, elem_expr)),
        delimited(char('['), digit1, char(']')),
    ))(input)
    .map(|(next, res)| {
        (next, PatternExpr::Elem(ElemExpr {
            child: Box::new(res.0),
            idx: res.1.parse::<usize>().unwrap(),
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
    alt((seq_expr, attr_expr, elem_expr, child_expr))(input)
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

fn expr_repr_token(input: &str) -> Res<&str, ReprPiece> {
    pattern_term(input)
    .map(|(next, res)| {
        (next, ReprPiece::Child(res))
    })
}

fn lit_repr_token(input: &str) -> Res<&str, ReprPiece> {
    let res = take_till1(|c| c == ' ' || c == '\n' || c == '$' || c == '`' || c == '#')(input)
    .map(|(next, res)| {
        Ok((next, ReprPiece::Literal(res.to_string())))
    });

    match res {
        Ok(inner_res) => inner_res,
        Err(err) => Err(err)
    }
}

fn space_repr_token(input: &str) -> Res<&str, ReprPiece> {
    many1_count(char(' '))(input)
    .map(|(next, res)| {
        (next, ReprPiece::Space(res))
    })
}

fn indent_repr_token(input: &str) -> Res<&str, ReprPiece> {
    many0_count(char(' '))(input)
    .map(|(next, res)| {
        (next, ReprPiece::Indent(res))
    })
}

#[allow(dead_code)]
fn newline_repr_token(input: &str) -> Res<&str, ReprPiece> {
    line_ending(input)
    .map(|(next, _)| {
        (next, ReprPiece::Newline)
    })
}

fn this_repr_token(input: &str) -> Res<&str, ReprPiece> {
    tag("$self")(input)
    .map(|(next, _)| {
        (next, ReprPiece::This)
    })
}

fn node_repr_token(input: &str) -> Res<&str, ReprPiece> {
    preceded(
        char('#'),
        digit1,
    )(input)
    .map(|(next, res)| {
        let idx = usize::from_str_radix(res, 10).unwrap();
        (next, ReprPiece::Node(idx))
    })
}

fn _repr_token(input: &str) -> Res<&str, ReprPiece> {
    alt((
        node_repr_token,
        this_repr_token,
        expr_repr_token,
        space_repr_token,
        lit_repr_token
    ))(input)
}

fn bare_repr_token(input: &str) -> Res<&str, (Vec<ReprPiece>, Vec<ReprAttr>)> {
    _repr_token(input)
    .map(|(next, res)| {
        (next, (vec![res], vec![]))
    })
}

fn repr_attrs(input: &str) -> Res<&str, Vec<ReprAttr>> {
    separated_list1(
        char(','),
        alt((
            tag("green"), tag("blue"), tag("red"), tag("yellow"), tag("orange"), tag("purple"), tag("underlined")
        ))
    )(input)
    .map(|(next, res)| {
        let attrs = res.iter().map(|r| match *r {
            "green" => ReprAttr::Green,
            "blue" => ReprAttr::Blue,
            "red" => ReprAttr::Red,
            "orange" => ReprAttr::Orange,
            "yellow" => ReprAttr::Yellow,
            "purple" => ReprAttr::Purple,
            "underlined" => ReprAttr::Underlined,
            _ => panic!()
        })
        .collect::<Vec<ReprAttr>>();

        (next, attrs)
    })
}

fn adorned_repr_token(input: &str) -> Res<&str, (Vec<ReprPiece>, Vec<ReprAttr>)> {
    let res = tuple((
        preceded(
            char('`'),
            terminated(
                take_until("`"),
                char('`')
            )
        ),
        preceded(char(':'), repr_attrs)
    ))(input)
    .map(|(next, res)| {
        let (_, toks) = many1(_repr_token)(res.0)?;
        Ok((next, (toks, res.1)))
    });

    match res {
        Ok(inner_res) => inner_res,
        Err(err) => Err(err)
    }
}

fn repr_token(input: &str) -> Res<&str, (Vec<ReprPiece>, Vec<ReprAttr>)> {
    alt((
        adorned_repr_token,
        bare_repr_token
    ))(input)
}

impl Repr {
    pub fn from_str(input: &str) -> Repr {
        separated_list1(
            line_ending,
            pair(indent_repr_token, many0(repr_token))
        )(input)
        .map(|(_, res)| {
            let mut all_pieces = vec![];

            for (idx, line_pieces) in res.iter().enumerate() {
                if idx > 0 {
                    all_pieces.push((line_pieces.0.clone(), vec![]));
                } else if res.len() == 1 {
                    // for single-line reprs append the indent piece as a space.
                    if let ReprPiece::Indent(n) = &line_pieces.0 {
                        if *n > 0 {
                            all_pieces.push((ReprPiece::Space(*n), vec![]));
                        }
                    }
                }

                for piece_pair in &line_pieces.1 {
                    let attrs = &piece_pair.1;

                    for piece in &piece_pair.0 {
                        all_pieces.push((piece.clone(), attrs.clone()));
                    }
                }

                // ignore trailing newlines.
                if res.len() > 1 && idx == res.len() - 2 && res[idx+1].0 == ReprPiece::Newline {
                    break;
                }

                // also ignore preceding newlines.
                if idx < res.len() - 1 && idx > 0 {
                    all_pieces.push((ReprPiece::Newline, vec![]));
                }
            }

            // println!("{:?}", all_pieces);

            Repr {
                pieces: all_pieces,
            }
        }).unwrap()
    }
}

impl Display for PatternExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternExpr::Attr(a) => write!(f, "{}.{}", &*a.child, a.attr),
            PatternExpr::Elem(e) => write!(f, "{}[{}]", &*e.child, e.idx),
            PatternExpr::Seq(s) => {
                let sep = if s.separator == "\n" { "<NEWLINE>" } else { &s.separator };
                write!(f, "${{{}*<{}>}}", s.child, sep)
            },
            PatternExpr::Child(c) => write!(f, "${{{}}}", c),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = self.lit.clone();

        for attr in &self.attrs {
            match attr {
                ReprAttr::Green => result = format!("\x1b[1m\x1b[32m{}", result),
                ReprAttr::Blue => result = format!("\x1b[1m\x1b[34m{}", result),
                ReprAttr::Red => result = format!("\x1b[1m\x1b[31m{}", result),
                ReprAttr::Orange => result = format!("\x1b[1m\x1b[38;5;208m{}", result),
                ReprAttr::Yellow => result = format!("\x1b[1m\x1b[33m{}", result),
                ReprAttr::Purple => result = format!("\x1b[1m\x1b[38;5;129m{}", result),
                ReprAttr::Underlined => result = format!("\x1b[4m{}", result),
            }
        }

        result = format!("{}\x1b[39m\x1b[22m\x1b[24m", result);
        write!(f, "{}", result)
    }
}

impl Printer {
    pub fn new() -> Self {
        Printer {
            lines: vec![vec![]],
            row: 0,
            col: 0,
            node_ranges: HashMap::default(),
			raw_node_ranges: HashMap::default(),
            raw_range_to_node: HashMap::default(),
            node_to_raw_range: HashMap::default(),
            token_ranges: HashMap::default(),
            node_starts: HashMap::default(),
            idx_stacks: HashMap::default(),
            attrs: HashMap::default(),
            node_stack: vec![],
            indent_level: 0,
            full_text: String::new(),
			raw_text: String::new(),
        }
    }

    pub fn reset(&mut self) {
        self.lines = vec![vec![]];
        self.row = 0;
        self.col = 0;
        self.node_ranges = HashMap::default();
        self.raw_node_ranges = HashMap::default();
        self.raw_range_to_node = HashMap::default();
        self.node_to_raw_range = HashMap::default();
        self.token_ranges = HashMap::default();
        self.idx_stacks = HashMap::default();
        self.node_starts = HashMap::default();
        self.attrs = HashMap::default();
        self.node_stack = vec![];
        self.indent_level = 0;
        self.full_text = String::new();
        self.raw_text = String::new();
    }

    fn set_indent_level(&mut self, level: usize) {
        self.indent_level = level;
    }

    #[allow(dead_code)]
    fn line_len(&self, row: usize) -> usize {
        self.lines[row].iter().map(|t| {
            t.lit.chars().count()
        }).sum()
    }

    pub fn start_node(&mut self, node: &HlilNode) {
        self.node_stack.push((node.idx, self.row, self.col, self.raw_text.len()));

        if let Some(desc) = node.descriptor.as_ref() {
            // self.node_starts.entry(desc.name().clone()).or_insert_with(Vec::new).push(self.raw_text.len());

            Python::with_gil(|py| {
                let superclasses_py = desc.inner.call_method0(py, "superclasses").unwrap();
                let superclasses = superclasses_py.downcast_bound::<PyList>(py).unwrap();

                for superclass in superclasses {
                    let superclass = superclass.downcast::<PyString>().unwrap().extract().unwrap();
                    self.node_starts.entry(superclass).or_insert_with(Vec::new).push(self.raw_text.len());
                }
            })

            // if let Some(super_class) = desc.superclass() {
            //     self.node_starts.entry(super_class.clone()).or_insert_with(Vec::new).push(self.raw_text.len());
            // }
        }
    }

    pub fn end_node(&mut self) {
        if let Some((idx, start_row, start_col, raw_idx)) = self.node_stack.pop() {
			self.raw_node_ranges.entry(raw_idx).or_insert_with(Vec::new).push((idx, self.raw_text.len()));
            self.raw_range_to_node.entry((raw_idx, self.raw_text.len())).or_insert_with(Vec::new).push(idx);
            self.node_to_raw_range.insert(idx, (raw_idx, self.raw_text.len()));

            let node_range = self.node_ranges
                .entry(idx)
                .or_insert_with(HashMap::default);

            if start_row < self.lines.len() - 1 {
                let line_len = self.lines[start_row].iter().map(|t| t.lit.chars().count()).sum();
                node_range.insert(start_row, (start_col, line_len));

                for col in start_col..line_len {
                    self.idx_stacks.entry((start_row, col)).or_insert_with(Vec::new).push(idx);
                }

                for row in (start_row + 1)..self.row {
                    let line_len = self.lines[row].iter().map(|t| t.lit.chars().count()).sum();
                    node_range.insert(row, (0, line_len));

                    for col in 0..line_len {
                        self.idx_stacks.entry((row, col)).or_insert_with(Vec::new).push(idx);
                    }
                }

                node_range.insert(self.row, (0, self.col));

                for col in 0..self.col {
                    self.idx_stacks.entry((self.row, col)).or_insert_with(Vec::new).push(idx);
                }
            } else {
                node_range.insert(start_row, (start_col, self.col));

                for col in start_col..self.col {
                    self.idx_stacks.entry((start_row, col)).or_insert_with(Vec::new).push(idx);
                }
            }
        }
    }

    pub fn put_token(&mut self, token: Token) {
        if token.lit == "" {
            return;
        }

        // println!("{}", token);
        if token.lit == "\n" {
			self.raw_text.push('\n');
            self.full_text.push('\n');
            self.lines.push(vec![]);
            self.row += 1;
            self.col = 0;
            return;
        }

        if self.col < self.indent_level && token.lit != " ".repeat(self.indent_level - self.col) {
            self.put_token_str(&" ".repeat(self.indent_level - self.col));
        }

        let start_col = self.col;

		self.raw_text.push_str(&token.lit);
        self.full_text.push_str(&format!("{}", token));

        let n = self.lines.len() - 1;
        self.lines[n].push(token.clone());
        self.col += token.lit.chars().count();

        let end_col = self.col;

        self.token_ranges
            .entry(self.row)
            .or_insert_with(Vec::new)
            .push((token, start_col, end_col));
    }

    pub fn put_token_str(&mut self, s: &String) {
        self.put_token(Token {
            lit: s.clone(),
            expr: None,
            attrs: vec![],
        })
    }
}

impl PatternExpr {
    pub fn evaluate_with(&self, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>, folded_exprs: Rc<RefCell<HashSet<NodeIndex>>>) -> (Option<AttrVal>, bool, bool) {
        match self {
            PatternExpr::Attr(AttrExpr {
                child: child_expr,
                attr,
            }) => {
                let (maybe_val, is_token, is_self_based) = (&*child_expr).evaluate_with(pool.clone(), ctx.clone(), folded_exprs.clone());

                match maybe_val {
                    Some(AttrVal::Node(idx)) => {
                        let result = (*pool.clone()).borrow().get(idx).get_attr(attr);
                        (result, !is_token, is_self_based)
                    },
                    _ => (None, false, false)
                }
            },
            PatternExpr::Elem(ElemExpr {
                child: child_expr,
                idx: elem_idx,
            }) => {
                let (maybe_val, _is_token, is_self_based) = (&*child_expr).evaluate_with(pool.clone(), ctx.clone(), folded_exprs.clone());

                match maybe_val {
                    Some(AttrVal::Node(seq_idx)) => {
                        let p = pool.clone();
                        let pb = (*p).borrow();
                        let seq = get_seq(&pb, seq_idx).unwrap();
                        (Some(AttrVal::Node(seq[*elem_idx])), false, is_self_based)
                    },
                    _ => (None, false, false)
                }
            },
            PatternExpr::Seq(SeqExpr {
                child: pat_child,
                separator: sep
            }) => {
                let p = pool.clone();
                let pb = (*p).borrow();
                let curr_idx = (*ctx.clone()).borrow().curr_node();

                let curr_node = curr_idx.map(|i| pb.get(i));
                let child_idx = curr_node.map(|n| n.get_child(&pat_child)).flatten();

                if let Some(HlilKind::Seq(seq)) = child_idx.map(|i| &pb.get(i).kind) {
                    (*ctx).borrow_mut().start_node(pb.get(child_idx.unwrap()));

                    let start_idx = (*ctx).borrow().printer.full_text.len();
                    let mut first_elem_occurred = false;

                    for (_, child_idx) in seq.iter().enumerate() {
                        let p = pool.clone();
                        let pb = (*p).borrow();
                        let child = pb.get(*child_idx);

                        let is_hidden = child.is_hidden;
                        // println!("{:?} {}", child.name, is_hidden);

                        if !is_hidden {
                            if first_elem_occurred {
                                (*ctx.clone()).borrow_mut().printer.put_token_str(&sep);
                            }

                            if sep == "\n" {
                                let indent_level = (*ctx.clone()).borrow().printer.indent_level;
                                let curr_col = (*ctx.clone()).borrow().printer.col;
                                (*ctx.clone()).borrow_mut().printer.put_token_str(&" ".repeat(indent_level.saturating_sub(curr_col)));
                            }

                            let _ = child.to_string(pool.clone(), ctx.clone(), folded_exprs.clone());
                            // println!("{:?}: {}", child.name, ss);
                            first_elem_occurred = true;
                        } else {
                            // let ss = child.str(pool.clone());
                            // println!("{} is hidden", ss);
                        }
                    }

                    let s = (*ctx).borrow().printer.full_text[start_idx..].to_string();
                    (*ctx).borrow_mut().end_node();
                    (Some(AttrVal::Str(s)), false, false)
                } else {
                    (None, false, false)
                }
            },
            PatternExpr::Child(pat_child) => {
                let p = pool.clone();
                let pb = (*p).borrow();

                let curr_idx = (*ctx.clone()).borrow().curr_node();
                let curr_node = curr_idx.map(|i| pb.get(i));

                if let Some(idx) = curr_node.map(|n| n.get_child(&pat_child)).flatten() {
                    (Some(AttrVal::Node(idx)), false, false)
                } else if pat_child == "self" {
                    (Some(AttrVal::Node(curr_idx.unwrap())), false, true)
                } else {
                    (None, false, false)
                }
            }
        }
        // (None, false)
    }
}

impl Repr {
    pub fn evaluate_with(&self, pool: Rc<RefCell<NodePool>>, ctx: Rc<RefCell<ReprContext>>, folded_exprs: Rc<RefCell<HashSet<NodeIndex>>>) -> String {
        let mut s = "".to_string();
        let orig_indent_level = (*ctx.clone()).borrow().printer.indent_level;

        for (cmd, attrs) in &self.pieces {
            // println!("{}", cmd);
            let (piece, expr, is_token) = match cmd {
                ReprPiece::Empty => ("".to_string(), None, false),
                ReprPiece::Newline => {
                    ("\n".to_string(), None, true)
                },
                ReprPiece::This => {
                    let p = pool.clone();
                    let pb = (*p).borrow();

                    let curr_idx = (*ctx.clone()).borrow().curr_node();
                    let curr_node = curr_idx.map(|i| pb.get(i));

                    let s = match curr_node.map(|n| &n.kind) {
                        Some(HlilKind::Var(name)) => name.clone(),
                        Some(HlilKind::Str((sval, _))) => sval.clone(),
                        Some(HlilKind::Data((_, sym))) => sym.clone(),
                        Some(_) => curr_node.unwrap().str(pool.clone()),
                        _ => panic!(),
                    };

                    (s, curr_idx, true)
                },
                ReprPiece::Space(n) => {
                    (" ".repeat(*n), None, true)
                },
                ReprPiece::Indent(n) => {
                    let new_indent_level = orig_indent_level + n;
                    // println!("indent from {} to {}", (*ctx).borrow().printer.indent_level, new_indent_level);
                    // (*ctx).borrow_mut().push_indent_level(new_indent_level);
                    (*ctx).borrow_mut().indent = new_indent_level;
                    (*ctx).borrow_mut().printer.set_indent_level(new_indent_level);
                    ("".to_string(), None, false)
                },
                ReprPiece::Literal(lit) => {
                    // HACK: Assume that any single-piece repr of an Expr is that Expr.
                    // NOTE: This doesn't work.
                    let p = pool.clone();
                    let pb = (*p).borrow();
                    let curr_idx = (*ctx.clone()).borrow().curr_node();
                    let curr_node = curr_idx.map(|i| pb.get(i));
                    let mut expr_idx = None;

                    // println!("{} {:?} {:?}", lit, self.pieces, curr_node);

                    if self.pieces.len() == 1 && curr_node.map(|n| n.descends_from(&"base.Expr".to_string())).unwrap_or(false) {
                        expr_idx = Some(curr_idx.unwrap());
                    }

                    (lit.clone(), expr_idx, true)
                },
                ReprPiece::Child(expr) => {
                    match &expr {
                        PatternExpr::Child(name) => {
                            let p = pool.clone();
                            let pb = (*p).borrow();

                            let curr_idx = (*ctx.clone()).borrow().curr_node();
                            let curr_node = curr_idx.map(|i| pb.get(i));

                            let child_idx = curr_node.map(|n| n.get_child(name)).flatten();

                            if let Some(child) = child_idx.map(|i| pb.get(i)) {
                                let child_str = child.to_string(pool.clone(), ctx.clone(), folded_exprs.clone());
                                // println!("{} {:?}", child_str, child);
                                (child_str, None, false)
                            } else {
                                ("".to_string(), None, false)
                            }
                        },
                        _ => {
                            let (maybe_val, is_token, is_self_based) = expr.evaluate_with(pool.clone(), ctx.clone(), folded_exprs.clone());

                            let (s, expr) = match maybe_val {
                                Some(AttrVal::Num(num)) => (format!("{}", num), None),
                                Some(AttrVal::Str(st)) => (st, None),
                                Some(AttrVal::Node(idx)) => {
                                    let s = (*pool).borrow().get(idx).to_string(pool.clone(), ctx.clone(), folded_exprs.clone());
                                    // ("".to_string(), Some(idx))
                                    (if is_self_based { s } else { "".to_string() }, Some(idx))
                                },
                                _ => ("".to_string(), None)
                            };

                            (s, expr, is_token && !is_self_based)
                        }
                    }
                },
                ReprPiece::Node(idx) => {
                    let p = pool.clone();
                    let pb = (*p).borrow();
                    let node = pb.get(*idx);
                    let node_s = node.to_string(pool.clone(), ctx.clone(), folded_exprs.clone());
                    (node_s, Some(*idx), false)
                },
            };

            let token = Token {
                lit: piece,
                expr: expr,
                attrs: attrs.clone(),
            };

            let col = (*ctx).borrow().printer.col;
            let il = (*ctx).borrow().printer.indent_level;

            if col < il && token.lit != " ".repeat(il - col) {
                s.push_str(&" ".repeat(il - col));
            }

            s.push_str(&format!("{}", &token));

            if is_token {
                (*ctx.clone()).borrow_mut().printer.put_token(token);
            }
        }

        // println!("{}", s);
        s
        // "".to_string()
    }
}

impl Default for ReprContext {
    fn default() -> Self {
        ReprContext {
            printer: Printer::new(),
            indent_stack: vec![],
            last_indent_idx: 0,
            indent: 0,
        }
    }
}

impl ReprContext {
    pub fn new() -> Self {
        ReprContext {
            printer: Printer::new(),
            indent_stack: vec![],
            last_indent_idx: 0,
            indent: 0,
        }
    }

    pub fn curr_node<'a>(&self) -> Option<NodeIndex> {
        self.printer.node_stack.last().map(|(i, _, _, _)| *i)
    }

    pub fn start_node(&mut self, node: &HlilNode) {
        self.printer.start_node(node);
    }

    pub fn end_node(&mut self) {
        self.printer.end_node();

        // println!("popping indent {:?} {}", self.indent_stack, self.last_indent_idx);
        // while self.indent_stack.len() > self.last_indent_idx {
        //     self.indent_stack.pop();
        // }

        // (self.indent, self.last_indent_idx) = if self.indent_stack.len() == 0 {
        //     (0, 0)
        // } else {
        //     self.indent_stack[self.indent_stack.len() - 1]
        // }
    }

    pub fn push_indent_level(&mut self, level: usize) {
        let prev_idx = self.last_indent_idx;
        self.last_indent_idx = self.indent_stack.len();
        self.indent_stack.push((self.indent, prev_idx));
        self.printer.set_indent_level(level);
        self.indent = level;
    }
}
