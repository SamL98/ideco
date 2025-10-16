use fxhash::FxHashMap;
use tinyjson::JsonValue;

use egui;
use egui::{Pos2, Rect};

use crate::pool::{NodeIndex, NodePool};
use crate::function::LiftingOptions;
use crate::lang::Language;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

pub type FunctionIndex = usize;
pub type WindowIndex = usize;

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum EditMode {
    Normal,
    Insert,
}

impl Default for EditMode {
    fn default() -> Self {
        EditMode::Normal
    }
}

#[derive(Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct RuleWindow {
    pub function: FunctionIndex,
    pub vars: FxHashMap<String, NodeIndex>,
    pub win: DecompilationWindow,
    pub was_selected: bool,
    pub yank: bool,
}

#[derive(Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct NotesWindow {
	// pub cmd_buf: String,
    pub text: String,
    pub was_selected: bool,
    pub yank: bool,
    pub menu_pos: Option<Pos2>,
    pub pasted: bool,
    pub snippets: FxHashMap<(FunctionIndex, NodeIndex), Vec<(f32, f32, f32, f32)>>,
    // pub mode: EditMode,
}

#[allow(dead_code)]
#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct DecompilationWindow {
	pub function: FunctionIndex,
	pub cmd_buf: String,
    pub curr_token_idx: usize,
    pub curr_token: usize,
    pub curr_line: usize,
    pub curr_col: usize,
    pub expr_idx: Option<usize>,
    pub show_menu: bool,
    pub show_options: bool,
    pub show_stack: bool,
    pub is_template_editor: bool,
    pub vars: FxHashMap<NodeIndex, String>,
    pub rule_repr: String,
	#[serde(skip)]
	pub token_rects: Vec<Vec<Rect>>,
	pub history: Vec<u64>,
	pub history_idx: usize,
	pub debug_tree: bool,
	pub edited_node: Option<NodeIndex>,
    pub template_idx: Option<NodeIndex>,
	pub started_editing: bool,
    #[serde(skip)]
    pub new_popover: Option<(u64, NodeIndex, f32, f32, Option<(Vec<NodeIndex>, Rc<RefCell<NodePool>>)>)>,
	pub expanded_calls: FxHashMap<(u64, NodeIndex), (WindowIndex, f32, f32, bool)>,
	pub committed_var: Option<(NodeIndex, String)>,
	pub show_rects: bool,
	pub show_xrefs: bool,
	pub new_parent: Option<(u64, u64)>,
	pub selected_range: Option<(NodeIndex, usize, i32, usize)>,
	pub was_selected: bool,
    pub yank: bool,
	#[serde(skip)]
    pub addr2func: HashMap<u64, HashMap<String, JsonValue>>,
    pub options: LiftingOptions,
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Window {
    Rule(DecompilationWindow),
    Notes(NotesWindow),
    Decomp(DecompilationWindow),
}

#[allow(dead_code)]
impl Window {
    pub fn was_selected(&self) -> bool {
        match self {
            Window::Rule(w) => w.was_selected,
            Window::Notes(w) => w.was_selected,
            Window::Decomp(w) => w.was_selected,
        }
    }

    pub fn set_selected(&mut self, s: bool) {
        match self {
            Window::Rule(w) => w.was_selected = s,
            Window::Notes(w) => w.was_selected = s,
            Window::Decomp(w) => w.was_selected = s,
        }
    }

    pub fn yank(&self) -> bool {
        match self {
            Window::Rule(w) => w.yank,
            Window::Notes(w) => w.yank,
            Window::Decomp(w) => w.yank,
        }
    }

    pub fn set_yank(&mut self, s: bool) {
        match self {
            Window::Rule(w) => w.yank = s,
            Window::Notes(w) => w.yank = s,
            Window::Decomp(w) => w.yank = s,
        }
    }
}

impl Default for DecompilationWindow {
	fn default() -> Self {
		DecompilationWindow {
			function: 0,
			cmd_buf: String::new(),
			curr_token_idx: 0,
			curr_token: 0,
			curr_line: 0,
			curr_col: 0,
			expr_idx: None,
            is_template_editor: false,
            vars: FxHashMap::default(),
            rule_repr: String::new(),
			show_menu: false,
			show_options: false,
			show_stack: false,
			token_rects: vec![],
			history: vec![],
			history_idx: 0,
			debug_tree: false,
			edited_node: None,
			template_idx: None,
			started_editing: false,
			expanded_calls: FxHashMap::default(),
            new_popover: None,
			committed_var: None,
			show_rects: false,
			show_xrefs: false,
			new_parent: None,
			selected_range: None,
			was_selected: false,
            yank: false,
            addr2func: HashMap::new(),
            options: LiftingOptions::default(),
		}
	}
}

#[allow(dead_code)]
impl DecompilationWindow {
	pub fn set_addr(&mut self, idx: FunctionIndex) {
		self.function = idx;
		self.cmd_buf.clear();
		self.curr_token_idx = 0;
		self.curr_token = 0;
		self.curr_line = 0;
		self.curr_col = 0;
		self.expr_idx = None;
		self.show_menu = false;
		self.show_options = false;
		self.show_stack = false;
		self.debug_tree = false;
		self.token_rects.clear();
		self.edited_node = None;
		self.template_idx = None;
	}

    #[allow(dead_code)]
	pub fn push_addr(&mut self, addr: u64) {
		self.history.truncate(self.history_idx + 1);
		self.history.push(addr);
		self.history_idx += 1;
	}

	pub fn has_next(&self) -> bool {
		self.history_idx < self.history.len().saturating_sub(1)
	}

	pub fn goto_next(&mut self, _lang: Rc<RefCell<Language>>) {
		if self.has_next() {
			self.history_idx += 1;
            todo!();
			// self.set_addr(self.history[self.history_idx], lang, None);
		}
	}

	pub fn has_prev(&self) -> bool {
		self.history_idx > 0
	}

	pub fn goto_prev(&mut self, _lang: Rc<RefCell<Language>>) {
		if self.has_prev() {
			self.history_idx -= 1;
            todo!();
			// self.set_addr(self.history[self.history_idx], lang, None);
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Ansi256(u8),
    Reset,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
struct Segment {
    pub text: String,
    pub color: Color,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Color::Reset      => write!(f, "\x1b[0m"),
            Color::Black      => write!(f, "\x1b[1m\x1b[30m"),
            Color::Red        => write!(f, "\x1b[1m\x1b[31m"),
            Color::Green      => write!(f, "\x1b[1m\x1b[32m"),
            Color::Yellow     => write!(f, "\x1b[1m\x1b[33m"),
            Color::Blue       => write!(f, "\x1b[1m\x1b[34m"),
            Color::Magenta    => write!(f, "\x1b[1m\x1b[35m"),
            Color::Cyan       => write!(f, "\x1b[1m\x1b[36m"),
            Color::White      => write!(f, "\x1b[1m\x1b[37m"),
            Color::Ansi256(n) => write!(f, "\x1b[1m\x1b[38;5;{}m", n),
        }
    }
}

#[allow(dead_code)]
fn parse_sgr(code: &str) -> Color {
    let mut parts: Vec<&str> = code.split(';').collect();
    if parts.is_empty() {
        return Color::Reset;
    }

    if parts[0] == "1" {
        let _ = parts.remove(0);
    }

    if parts.is_empty() {
        return Color::Reset;
    }

    match parts[0] {
        "0" => Color::Reset,
        "30" => Color::Black,
        "31" => Color::Red,
        "32" => Color::Green,
        "33" => Color::Yellow,
        "34" => Color::Blue,
        "35" => Color::Magenta,
        "36" => Color::Cyan,
        "37" => Color::White,
        "38" if parts.len() >= 3 && parts[1] == "5" => {
            if let Ok(n) = parts[2].parse::<u8>() {
                Color::Ansi256(n)
            } else {
                panic!()
            }
        }
        _ => panic!(),
    }
}

#[allow(dead_code)]
fn parse_ansi(input: &str) -> Vec<Segment> {
    let mut segments = Vec::new();
    let mut current_attr = Color::Reset;
    let mut buf = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            if let Some('[') = chars.peek() {
                chars.next();
                let mut code = String::new();

                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphabetic() {
                        let finalizer = chars.next().unwrap();
                        if finalizer == 'm' {
                            if !buf.is_empty() {
                                segments.push(Segment {
                                    text: buf.clone(),
                                    color: current_attr.clone(),
                                });
                                buf.clear();
                            }
                            current_attr = parse_sgr(&code);
                        }
                        break;
                    } else {
                        code.push(c);
                        chars.next();
                    }
                }
            } else {
                buf.push(ch);
            }
        } else {
            buf.push(ch);
        }
    }

    if !buf.is_empty() {
        segments.push(Segment {
            text: buf,
            color: current_attr,
        });
    }

    segments
}

#[allow(dead_code)]
impl RuleWindow {
    pub fn new(function: FunctionIndex) -> Self {
        let mut win = Self {
            function, 
            ..Default::default()
        };

        win.win.set_addr(function);

        win
    }
}
