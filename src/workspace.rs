use crate::pool::{NodeIndex, NodePool};
use crate::lang::Language;
use crate::windows::{NotesWindow, DecompilationWindow, Window, FunctionIndex, WindowIndex};
use crate::function::*;

use tinyjson::JsonValue;

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(serde::Deserialize, serde::Serialize)]
pub struct WindowInstance {
    pub window: Window,
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
    pub is_top_level: bool,
    pub is_visible: bool,
}

#[derive(serde::Deserialize, serde::Serialize)]
pub struct Workspace {
	pub name: String,
    pub notes: NotesWindow,
	pub functions: Vec<Function>,
	pub windows: Vec<WindowInstance>,
	pub selected_window: usize,
	pub closed_windows: Vec<usize>,
    pub query: String,
    pub query_results: Vec<(String, u64)>,
	pub paste_buffer: Option<(FunctionIndex, NodeIndex, usize, i32, usize)>,
    pub show_left_panel: bool,
    pub show_right_panel: bool,

	#[serde(skip)]
    pub name2addr: HashMap<String, u64>,

	#[serde(skip)]
    pub addr2func: HashMap<u64, HashMap<String, JsonValue>>,
}

impl Default for Workspace {
	fn default() -> Self {
		Workspace {
			name: String::new(),
            notes: NotesWindow::default(),
			functions: vec![],
			windows: vec![],
			selected_window: 0,
			closed_windows: vec![],
            query: String::new(),
            query_results: vec![],
            paste_buffer: None,
            show_left_panel: false,
            show_right_panel: false,
            name2addr: HashMap::new(),
            addr2func: HashMap::new(),
		}
	}
}

#[allow(dead_code)]
impl DecompilationWindow {
    pub fn function<'a>(&self, ws: &'a Workspace) -> &'a Function {
        &ws.functions[self.function]
    }

    pub fn function_mut<'a>(&self, ws: &'a mut Workspace) -> &'a mut Function {
        ws.functions.get_mut(self.function).unwrap()
    }

	pub fn rerender(&mut self, func: &mut Function) {
		self.curr_token_idx = 0;
		self.curr_token = 0;
		self.expr_idx = None;
		self.selected_range = None;
		self.token_rects.clear();
        let _ = func.rerender();
	}

	pub fn lift(&mut self, func: &mut Function, lang: Rc<RefCell<Language>>) {
		self.curr_token_idx = 0;
		self.curr_token = 0;
		self.expr_idx = None;
		self.selected_range = None;
		self.token_rects.clear();

        func.repr_ctx = get_repr_context(func.pool.clone(), func.root, lang);
        let _ = func.rerender();
	}
}

impl Window{
    #[allow(dead_code)]
    pub fn title(&self, ws: &Workspace) -> String {
        match self {
            Window::Rule(_) => "rule".to_string(),
            Window::Notes(_) => "Notes".to_string(),
            Window::Decomp(w) => format!("0x{:x}", w.function(ws).addr),
        }
    }
}

#[allow(dead_code)]
impl Workspace {
	pub fn new(
        name: &str,
        name2addr: HashMap<String, u64>,
        addr2func: HashMap<u64, HashMap<String, JsonValue>>,
    ) -> Self {
        let mut ws = Workspace::default();
        ws.name = name.to_string();
        ws.name2addr = name2addr;
        ws.addr2func = addr2func;
        ws
	}

    pub fn close_selected(&mut self) {
        self.closed_windows.push(self.selected_window)
    }

	pub fn display_function(
		&mut self,
        func_idx: FunctionIndex,
		x: f32,
		y: f32,
        w: f32,
        h: f32,
		is_top_level: bool,
        is_visible: bool,
	) -> WindowIndex {
		let mut new_window = DecompilationWindow::default();
		new_window.set_addr(func_idx);
        new_window.addr2func = self.addr2func.clone();

        let instance = WindowInstance {
            window: Window::Decomp(new_window),
            x: x,
            y: y,
            w: w,
            h: h,
            is_top_level: is_top_level,
            is_visible: is_visible,
        };

		if let Some(avail_idx) = self.closed_windows.pop() {
			self.selected_window = avail_idx;
			self.windows[avail_idx] = instance;
            avail_idx
		} else {
			self.selected_window = self.windows.len();
			self.windows.push(instance);
            self.windows.len() - 1
		}
    }

	pub fn open_function(
		&mut self,
		addr: u64,
		x: f32,
		y: f32,
        w: f32,
        h: f32,
		lang: Rc<RefCell<Language>>,
		params: Option<(Vec<NodeIndex>, Rc<RefCell<NodePool>>)>,
		is_top_level: bool,
        is_visible: bool,
	) -> WindowIndex {
        let func_json = &self.addr2func[&addr];
        let func = Function::new(addr, func_json, lang, params);
        let func_idx = self.functions.len();
        self.functions.push(func);
        self.display_function(func_idx, x, y, w, h, is_top_level, is_visible)
	}

    pub fn window<'a> (&'a self, ix: WindowIndex) -> &'a Window {
        &self.windows[ix].window
    }

    pub fn window_mut<'a> (&'a mut self, ix: WindowIndex) -> &'a mut Window {
        &mut self.windows.get_mut(ix).unwrap().window
    }
}
