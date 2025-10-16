use markdown;
use markdown::mdast::Node as mdnode;

use pyo3::{Py, PyAny, Python};
use pyo3::types::*;
use pyo3::prelude::PyModule;

use eframe;
use eframe::egui;
use egui::epaint::*;
use egui::{vec2, pos2, Color32, Key};

use crate::app::App;
use crate::workspace::*;
use crate::windows::*;
use crate::function::Function;
use crate::pool::*;
use crate::hlil::HlilNode;
use crate::lang::*;
use crate::repr::{ReprAttr, AttrVal};
// use crate::template::TemplateContext;
use crate::hlil::HlilKind;
use crate::Args;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::Ref;
use std::cell::RefCell;
use std::fs;

#[allow(deprecated)]
impl eframe::App for App {
	fn save(&mut self, storage: &mut dyn eframe::Storage) {
		eframe::set_value(storage, eframe::APP_KEY, self);
	}

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		// self.do_bullshit_serde_init();

		let frame = egui::Frame::none()
			.inner_margin(Margin::same(5.0))
			.fill(Color32::from_rgb(46, 46, 46));

        let res = egui::TopBottomPanel::top("workspaces")
            .frame(egui::Frame::none().inner_margin(4.0))
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    for (i, workspace) in self.workspaces.iter().enumerate() {
                        if ui.selectable_label(i == self.selected_idx, workspace.name.as_str()).clicked() {
                            self.selected_idx = i;
                        }
                    }

                    if ui.button("+").clicked() {
                        let ws_name = format!("Workspace {}", self.workspaces.len() + 1);
                        self.workspaces.push(Workspace::new(ws_name.as_str(), self.name2addr.clone(), self.addr2func.clone(), self.options.clone()));
                        self.selected_idx = self.workspaces.len() - 1;
                    }

                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if ui.button("R").clicked() {
                            if self.selected_idx < self.workspaces.len() {
                                self.workspaces[self.selected_idx].show_right_panel ^= true;
                            }
                        }

                        if ui.button("L").clicked() {
                            if self.selected_idx < self.workspaces.len() {
                                self.workspaces[self.selected_idx].show_left_panel ^= true;
                            }
                        }
                    });
                });
            });

		egui::CentralPanel::default().frame(frame).show(ctx, |ui| {
            if self.selected_idx >= self.workspaces.len() {
                return;
            }

			let ws = &mut self.workspaces[self.selected_idx];
            ws.display(ctx, ui, self.lang.clone(), res.response.rect.height())
		});
	}
}

#[allow(deprecated)]
impl Workspace {
    pub fn display(&mut self, ctx: &egui::Context, ui: &mut egui::Ui, lang: Rc<RefCell<Language>>, top_bar_height: f32) {
		let frame = egui::Frame::none()
			.inner_margin(Margin::same(5.0))
			.fill(Color32::from_rgb(46, 46, 46));

        let mut left_sidebar_size = 0.0;
        let mut right_sidebar_size = 0.0;
        let mut new_func = None;

        if self.show_left_panel {
            let res = egui::SidePanel::left("Search").frame(frame).show(ctx, |ui| {
                let old_query = self.query.clone();

                ui.vertical(|ui| {
                    let _ = ui.text_edit_singleline(&mut self.query).has_focus();

                    if self.query != old_query {
                        self.query_results.clear();

                        if self.query.len() >= 3 {
                            for (name, addr) in &self.name2addr {
                                if name.contains(&self.query) {
                                    self.query_results.push((name.clone(), *addr));
                                }
                            }
                        }
                    }

                    egui::ScrollArea::both().show(ui, |ui| {
                        for (name, addr) in &self.query_results {
                            let label = egui::Label::new(egui::RichText::new(name).color(Color32::WHITE)).extend();

                            if ui.add(label).clicked() {
                                new_func = Some(*addr);
                            }
                        }
                    });
                });
            });

            left_sidebar_size = res.response.rect.width();
        }

		let mut double_clicked_window = None;
        let mut new_popovers = vec![];
        let mut popover_info = vec![];
        let paste_buffer = self.paste_buffer.clone();

        if self.show_right_panel {
            let res = egui::SidePanel::right("Notes").frame(frame).show(ctx, |ui| {
                self.notes.display(
                    ctx,
                    ui,
                    lang.clone(),
                    false,
                    &self.name,
                    paste_buffer,
                    &mut self.functions
                );
            });

            right_sidebar_size = res.response.rect.width();
        }

        if let Some(addr) = new_func {
            let w = ui.available_size().x - left_sidebar_size - right_sidebar_size;
            let h = ui.available_size().y;
            self.open_function(addr, left_sidebar_size, top_bar_height, w, h, lang.clone(), None, true, true);
        }

        let funcs = &mut self.functions;
        let mut template = None;

		egui::CentralPanel::default().frame(frame).show(ctx, |_| {
			let mut new_selected = self.selected_window;

			for (i, inst) in self.windows.iter_mut().enumerate() {
				if self.closed_windows.contains(&i) || !inst.is_visible {
					continue;
				}

				let mut open = true;
				// println!("{}", ui.available_size());

                let title = match &inst.window {
                    Window::Rule(_) => "Rule".to_string(),
                    Window::Notes(_) => "Notes".to_string(),
                    Window::Decomp(w) => format!("0x{:x}", funcs[w.function].addr),
                };

				let mut win = egui::Window::new(title)
					.frame(egui::Frame::none()
						.fill(Color32::DARK_GRAY)
						.inner_margin(Margin::same(5.0))
						.rounding(Rounding::same(8.0)));

				if inst.is_top_level {
					win = win.default_pos(pos2(inst.x, inst.y));
				} else {
					win = win.fixed_pos(pos2(inst.x, inst.y));
				}

				win
					.default_size(vec2(inst.w, inst.h))
					.order(egui::Order::Foreground)
					.open(&mut open)
					.movable(inst.is_top_level)
					.collapsible(inst.is_top_level)
					.title_bar(inst.is_top_level)
					.show(ctx, |ui| {
                        if let Window::Decomp(ref mut win) = &mut inst.window {
                            if inst.is_top_level {
                                egui::menu::bar(ui, |ui| {
                                    if ui.menu_button("<", |_| {}).response.clicked() && win.has_prev() {
                                        win.goto_prev(lang.clone());
                                    }

                                    if ui.menu_button(">", |_| {}).response.clicked() && win.has_next() {
                                        win.goto_next(lang.clone());
                                    }

                                    ui.toggle_value(&mut win.debug_tree, "Debug");
                                });
                            }
                        }

						frame.fill(Color32::from_rgb(46, 46, 46)).show(ui, |ui| {
                            match &mut inst.window {
                                Window::Decomp(ref mut win) => {
                                    if win.debug_tree {
                                        ui.horizontal(|ui| {
                                            win.display_decomp(
                                                ctx,
                                                ui,
                                                lang.clone(),
                                                i == self.selected_window,
                                                funcs
                                            );
                                            win.display_tree(
                                                ctx,
                                                ui,
                                                lang.clone(),
                                                i == self.selected_window,
                                                funcs
                                            );
                                        });

                                    } else {
                                        win.display_decomp(
                                            ctx,
                                            ui,
                                            lang.clone(),
                                            i == self.selected_window,
                                            funcs
                                        );
                                    }
                                },
                                Window::Rule(ref mut win) => {
                                    win.display_decomp(ctx, ui, lang.clone(), i == self.selected_window, funcs);
                                },
                                _ => (),
                            }
						});

						if ui.input(|input| input.pointer.button_double_clicked(egui::PointerButton::Primary)) && ui.ui_contains_pointer() {
							double_clicked_window = Some(i);
						}

                        if let Window::Decomp(ref mut win) = &mut inst.window {
                            if let Some(node_idx) = win.template_idx.take() {
                                template = Some((win.function, node_idx));
                            }
                        }
					});

				if inst.window.was_selected() {
					inst.window.set_selected(false);
					new_selected = i;
				}

                if inst.window.yank() {
                    inst.window.set_yank(false);

                    if let Window::Decomp(ref mut decomp_window) = &mut inst.window {
                        if let Some((node_idx, start_idx, delta, n)) = decomp_window.selected_range.take() {
                            self.paste_buffer = Some((decomp_window.function, node_idx, start_idx, delta, n));
                        }
                    }
                }

				if !open {
					self.closed_windows.push(i);
				}

                if let Window::Decomp(ref mut decomp_window) = &mut inst.window {
                    if let Some(p) = decomp_window.new_popover.take() {
                        new_popovers.push((i, p));
                    }

                    for (_, (ix, x, y, is_visible)) in &decomp_window.expanded_calls {
                        popover_info.push((*ix, *x, *y, *is_visible));
                    }
                } else if let Window::Notes(ref mut note_window) = &mut inst.window {
                    if note_window.pasted {
                        self.paste_buffer = None;
                        note_window.pasted = false;
                    }
                }
			}

			self.selected_window = new_selected;
		});

        if let Some((func_idx, node_idx)) = template {
            let func = &funcs[func_idx];
            let new_func = func.spawn_child(node_idx, lang.clone());
            let func_idx = self.functions.len();
            self.functions.push(new_func);

            let mut window = DecompilationWindow::default();
            window.set_addr(func_idx);
            window.is_template_editor = true;
            let mut rule_window = Window::Rule(window);
            rule_window.set_selected(true);

            self.windows.push(WindowInstance {
                window: rule_window,
                x: 50.0,
                y: 50.0,
                w: 50.0,
                h: 50.0,
                is_top_level: true,
                is_visible: true,
            });
            self.selected_window = self.windows.len() - 1;
        }

        if self.selected_window >= self.windows.len() {
            return;
        }

        if !matches!(self.windows[self.selected_window].window, Window::Decomp(_)) {
            return;
        }

        let Window::Decomp(ref mut curr_win) = self.windows[self.selected_window].window else { panic!() };

		if let Some((_func_addr, call_addr)) = curr_win.new_parent.take() {
            self.close_selected();
            let inst = &self.windows[self.selected_window];
            let Window::Decomp(ref win) = &inst.window else { panic!() };
            let x = inst.x;
            let y = inst.y;
            let w = inst.w;
            let h = inst.h;

			let target = win.function(self).addr;
            let new_parent = self.open_function(target, inst.x, inst.y, inst.w, inst.h, lang.clone(), None, true, true);

            let Window::Decomp(ref parent) = self.window(new_parent) else { panic!() };
			let func = parent.function(self);

			let p = func.pool.clone();
            let p_ = func.pool.clone();
			let pb = (*p).borrow();

			let root = pb.get(func.root);

			if let Some(sym_idx) = root.find(p.clone(), &|node: &HlilNode| {
				if node.parents.iter().find(|par| (*p).borrow().get(**par).addr == Some(call_addr)).is_none() {
					return false;
				}

				let refs = node.references(p.clone());

				if refs.len() != 1 {
					return false;
				}

				refs[0] == target
			}) {
				if let Some(call_idx) = pb.get(sym_idx).parents.iter().find(|par| (*p).borrow().get(**par).addr == Some(call_addr)) {
					let params = pb.get(*call_idx).get_child(&"params".to_string()).map(|ix| get_seq(&pb, ix)).flatten().unwrap_or(vec![]);

					if let Some(stmt_idx) = pb.get(*call_idx).parents.iter().nth(0) {
                        let win_params = Some((params, p_));
						let old_win = self.open_function(target, x, y, w, h, lang.clone(), win_params, true, true);

                        let Window::Decomp(ref mut parent) = self.window_mut(new_parent) else { panic!() };
						parent.expanded_calls.insert((target, *stmt_idx), (old_win, x, y, true));
					}
				}
			}
		}

        for (i, (tgt, stmt_idx, x, y, params)) in new_popovers {
            let ix = self.open_function(tgt, x, y, 250.0, 150.0, lang.clone(), params, false, true);
            if let Window::Decomp(decomp_window) = self.window_mut(i) {
                decomp_window.expanded_calls.insert((tgt, stmt_idx), (ix, x, y, true));
            }
        }

        for (ix, x, y, is_visible) in popover_info {
            self.windows[ix].x = x;
            self.windows[ix].y = y;
            self.windows[ix].is_visible = is_visible;
        }

        let mut popover_infos = vec![];
        let mut non_empty = true;
        let funcs = &mut self.functions;

        while non_empty {
            popover_infos.clear();
            non_empty = false;

            for inst in self.windows.iter_mut() {
                if !inst.is_top_level {
                    let Window::Decomp(window) = &inst.window else { panic!() };
                    let edited_node = window.edited_node.as_ref().map(|ix| funcs[window.function].var_map.get(ix).map(|i| *i)).flatten();
                    let curr_var = window.get_selected_var(funcs).as_ref().map(|ix| funcs[window.function].var_map.get(ix).map(|i| *i)).flatten();
                    let committed_var = match &window.committed_var {
                        Some((ix, new_name)) => Some((funcs[window.function].var_map.get(ix).map(|i| *i), new_name.clone())),
                        _ => None,
                    };

                    non_empty |= edited_node.is_some() || curr_var.is_some() || committed_var.is_some();
                    popover_infos.push((edited_node, window.cmd_buf.clone(), curr_var, committed_var));
                } else {
                    popover_infos.push((None, String::new(), None, None))
                }
            }

            for inst in self.windows.iter_mut() {
                if let Window::Decomp(ref mut window) = &mut inst.window {
                    window.propagate_popovers(&popover_infos, funcs);
                    window.committed_var = None;
                }
            }
        }
	}
}

fn display_node(idx: NodeIndex, pool: &Ref<NodePool>, ui: &mut egui::Ui, indent: usize) {
	use HlilKind::*;
	let node = pool.get(idx);
	let indent_str = " ".repeat(indent * 2);

	match &node.kind {
		Int(int) => { ui.label(format!("{}Int(0x{:x})", indent_str, int)); },
		Float(float) => { ui.label(format!("{}Float({})", indent_str, float)); },
		Str((s, _)) => { ui.label(format!("{}{}", indent_str, s)); },
		Var(name) => { ui.label(format!("{}Var({})", indent_str, name)); },
		Opcode(op) => { ui.label(format!("{}Opcode({})", indent_str, op)); },
		Data((addr, _)) => { ui.label(format!("{}Data(0x{:x})", indent_str, addr)); },
		Seq(elems) => {
			ui.label(format!("{}Seq", indent_str));

			for elem in elems {
				display_node(*elem, pool, ui, indent + 1);
			}
		},
		Tree(tree) => {
			ui.label(format!("{}{}", indent_str, node.name.as_ref().unwrap()));

			for (name, child) in tree {
				ui.label(format!("{}{}:", " ".repeat((indent + 1) * 2), name));
				display_node(*child, pool, ui, indent + 1);
			}
		},
	}
}

fn calculate_sidebar_size(ctx: &egui::Context) -> Vec2 {
	ctx.fonts(|fonts|
		fonts.layout_no_wrap(">".to_string(), egui::FontId::monospace(9.0), Color32::BLACK).rect.size()
	)
}

pub fn get_ref(
	func: &Function,
	expr_idx: Option<NodeIndex>,
	line: usize,
	col: usize
) -> Option<(u64, Vec<NodeIndex>, Option<NodeIndex>)> {
	let rc = func.repr_ctx.clone();
	let rcb = (*rc).borrow();

	if let Some(selected_idx) = expr_idx {
		let p = func.pool.clone();
		let pb = (*p).borrow();
		let node = pb.get(selected_idx);
		let refs = node.references(func.pool.clone());

		if refs.len() == 1 {
			let mut params = vec![];

			// TODO: Make this use the "inputs" property of the node's descriptor so it's not dependent on hlil.Call.
			// TODO: Add a parent index to the Node so we don't have to do this.
			let idx_stack = &rcb.printer.idx_stacks[&(line, col)];
			let mut parent_idx = None;
			let mut stmt_idx = None;

			for (i, idx) in idx_stack.iter().enumerate() {
				if *idx == selected_idx && i < idx_stack.len() - 1 {
					parent_idx = Some(idx_stack[i + 1]);
					// break;
				}
				if pb.get(*idx).descends_from(&"base.Stmt".to_string()) {
					stmt_idx = Some(*idx);
					break;
				}
			}

			if let Some(parent_idx) = parent_idx {
				let parent = pb.get(parent_idx);

				if parent.descends_from(&"hlil.Call".to_string()) {
					if let HlilKind::Tree(children) = &parent.kind {
						params = get_seq(&pb, children["params"]).unwrap();
					}
				}
			}

			Some((refs[0], params, stmt_idx))
		} else if refs.len() == 0 {
			None
		} else {
			todo!("{:?}", refs);
		}
	} else {
		None
	}
}

fn calculate_rects(
	func: &Function,
	edited_node: Option<NodeIndex>,
	cmd_buf_len: usize,
	ctx: &egui::Context,
	job: egui::text::LayoutJob,
	t: Vec2,
	skip_chars: Vec<Vec<bool>>,
) -> Vec<Vec<egui::Rect>> {
	let rc = func.repr_ctx.clone();
	let rcb = (*rc).borrow();

	let mut token_rects = vec![];

	ctx.fonts(|fonts| {
		let galley = fonts.layout_job(job);

		for (i, row) in galley.rows.iter().enumerate() {
			let mut line_rects = vec![];
			let mut token_rect = row.glyphs[0].logical_rect();
			let mut token_len = 0;
			let mut j = 0;

			for (glyph_idx, glyph) in row.glyphs[2..].iter().enumerate() {
				if skip_chars[i][glyph_idx] {
					continue;
				}

				let token = &rcb.printer.lines[i][j];

				let lit_len = if edited_node.is_some() && token.expr == edited_node {
					cmd_buf_len
				} else {
					token.lit.len()
				};

				if token_len == lit_len {
					line_rects.push(token_rect.translate(t.clone()));
					token_rect = glyph.logical_rect();
					token_len = 0;
					j += 1;

					while j < rcb.printer.lines[i].len() && rcb.printer.lines[i][j].lit.len() == 0 {
						line_rects.push(Rect::NOTHING);
						j += 1
					}
				}

				token_rect = token_rect.union(glyph.logical_rect());
				token_len += 1;
			}

			line_rects.push(token_rect.translate(t.clone()));
            // for tok in &rcb.printer.lines[i] {
            //     println!("  \"{}\"", tok);
            // }
			token_rects.push(line_rects);
		}
	});

	token_rects
}

#[allow(deprecated)]
#[allow(dead_code)]
impl RuleWindow {
	pub fn display(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        lang: Rc<RefCell<Language>>,
        is_selected: bool,
        funcs: &mut Vec<Function>,
    ) {
        // println!("{}", funcs[self.function].rerender());
        // let mut win = DecompilationWindow::default();

        // self.handle_keyboard_input(ctx, lang.clone(), funcs);

        let widget = DecompilationWidget {
            ctx: ctx,
            win: self.win.clone(),
            lang: lang.clone(),
            is_selected,
            funcs: funcs,
        };

        // let rect = Rect::from_min_max(pos2(0.0, 0.0), pos2(100.0, 100.0));

		let frame = egui::Frame::none()
			.inner_margin(Margin::same(2.0))
			.fill(Color32::from_rgb(46, 46, 46));

		frame.show(ui, |ui| {
            // ui.put(rect, widget);
            // ui.allocate_ui_at_rect(rect, |ui| {
            //     ui.vertical(|ui| {
            //         ui.add(widget);
            //     });
            // });
            ui.add(widget);
        });

        if ui.input(|input| input.pointer.any_click()) && ui.ui_contains_pointer() {
            self.was_selected = true;
        }
    }

    // fn handle_keyboard_input(
    //     &mut self,
    //     ctx: &egui::Context,
    //     lang: Rc<RefCell<Language>>,
    //     funcs: &mut Vec<Function>,
    // ) {
		// let func = &mut funcs[self.win.function];

		// ctx.input(|input| {
    //         if input.key_released(Key::Delete) {
    //             let node_idx = if let Some((_node_idx, _elem_idx, _delta, _seq_len)) = self.win.selected_range.as_ref() {
    //                 todo!()
    //             } else if let Some(idx) = self.win.expr_idx {
    //                 let rc = func.repr_ctx.clone();
    //                 let rcb = (*rc).borrow();
    //                 rcb.printer.idx_stacks[&(self.win.curr_line, self.win.curr_col)][idx]
    //             } else {
    //                 return;
    //             };

    //             let name = format!("${}", self.vars.len());
    //             let dt = make_data_type(&mut (*func.pool).borrow_mut(), "".to_string(), 8, &*lang);
    //             let dt = AttrVal::Node(dt);
    //             let var = make_var(&mut (*func.pool).borrow_mut(), name.clone(), dt, &*lang);
    //             self.vars.insert(name, var);
    //             (*func.pool).borrow_mut().replace(node_idx, var);
    //             println!("{}", func.rerender());

    //             self.win.cmd_buf.clear();
    //             self.win.edited_node = Some(var);
    //             self.win.cmd_buf += "$_";
    //             self.win.started_editing = false;
    //             self.win.selected_range = None;
    //             ctx.request_repaint();
    //         }
    //     });
    // }
}

#[allow(deprecated)]
impl DecompilationWindow {
	pub fn get_selected_expr(&self, funcs: &mut Vec<Function>) -> Option<NodeIndex> {
		let func = &funcs[self.function];
		let rc = func.repr_ctx.clone();
		let rcb = (*rc).borrow();

		match self.expr_idx {
			Some(idx) => Some(rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][idx]),
			None => rcb.printer.lines[self.curr_line][self.curr_token].expr,
		}
	}

	pub fn get_selected_var(&self, funcs: &mut Vec<Function>) -> Option<NodeIndex> {
		let func = &funcs[self.function];

		let expr_idx = {
			let rc = func.repr_ctx.clone();
			let rcb = (*rc).borrow();

			if self.curr_line < rcb.printer.lines.len() &&
			   self.curr_token < rcb.printer.lines[self.curr_line].len() {
				rcb.printer.lines[self.curr_line][self.curr_token].expr
			} else {
				None
			}
		};

		if let Some(expr_idx) = expr_idx {
			if matches!((*func.pool).borrow().get(expr_idx).kind, HlilKind::Var(_)) {
				Some(expr_idx)
			} else {
				None
			}
		} else {
			None
		}
	}

    #[allow(dead_code)]
	pub fn get_selected_ref(&self, funcs: &mut Vec<Function>) -> Option<(u64, f32, f32, Vec<NodeIndex>)> {
		let func = &funcs[self.function];

		let expr_idx = {
			let rc = func.repr_ctx.clone();
			let rcb = (*rc).borrow();
			rcb.printer.lines[self.curr_line][self.curr_token].expr
		};

		if let Some((target, params, _)) = get_ref(func, expr_idx, self.curr_line, self.curr_col) {
			let rect = &self.token_rects[self.curr_line][self.curr_token_idx];
			Some((target, rect.right() + 10.0, rect.bottom() + 10.0, params))
		} else {
			None
		}
	}

	pub fn display_tree(
        &mut self,
        _ctx: &egui::Context,
        ui: &mut egui::Ui,
        _lang: Rc<RefCell<Language>>,
        _is_selected: bool,
        funcs: &mut Vec<Function>,
    ) {
		let func = &funcs[self.function];

		let p = func.pool.clone();
		let pb = (*p).borrow();

		ui.vertical(|ui| {
			egui::ScrollArea::both()
			.id_source(format!("{}_debug", func.addr))
			.show(ui, |ui| {
				display_node(func.root, &pb, ui, 0);
			});
		});
	}

	fn format_text(
		&mut self,
		sidebar_size: &Vec2,
        funcs: &mut Vec<Function>,
	) -> (
		egui::text::LayoutJob,
		Vec<Vec<usize>>,
		Vec<Vec<usize>>,
		Vec<Vec<bool>>,
		Vec<Vec<Option<(u64, Vec<NodeIndex>, NodeIndex)>>>,
		Vec<(usize, usize)>,
	) {
		let func = &funcs[self.function];
		let mut indent_level = 0;

		let mut job = egui::text::LayoutJob::default();
		job.wrap.max_width = f32::INFINITY;
        job.halign = egui::Align::LEFT;

		let mut expr_tokens = vec![];
		let mut call_tokens = vec![];
		let mut cols = vec![];

		let mut foldable_lines = vec![];
		let mut skip_chars = vec![];

		let rc = func.repr_ctx.clone();
		let rcb = (*rc).borrow();
		let num_lines = rcb.printer.lines.len();

		let p = func.pool.clone();
		let pb = (*p).borrow();

		for (i, line) in rcb.printer.lines.iter().enumerate() {
			let mut line_tokens = vec![];
			let mut line_cols = vec![];
			let mut line_skips = vec![];
			let mut line_calls = vec![];
			let mut line_len = 0;

			for (j, token) in line.iter().enumerate() {
				let mut fmt = egui::TextFormat::simple(egui::FontId::monospace(9.0), Color32::from_rgb(214, 214, 214));

				if j == 0 && token.lit.replace(" ", "").len() == 0 {
					let new_indent_level = token.lit.len();

					if new_indent_level > indent_level {
						let expr_idx = rcb.printer.idx_stacks[&(i, 0)][0];
						foldable_lines.push((i, expr_idx));
					}

					indent_level = new_indent_level;
				}

				if j == 0 {
					job.append("  ", sidebar_size.x, fmt.clone());
				}

				if let Some((target, params, Some(stmt_idx))) = get_ref(func, token.expr, i, line_len) {
					job.append("  ", 0.0, fmt.clone());
					line_skips.push(true);
					line_skips.push(true);
					line_calls.push(Some((target, params, stmt_idx)));
				} else {
					line_calls.push(None);
				}

				let text = if self.edited_node.is_some() && token.expr == self.edited_node {
					&self.cmd_buf
				} else {
					&token.lit
				};

				for _ in 0..text.len() {
					line_skips.push(false);
				}

				for attr in &token.attrs {
					match attr {
						ReprAttr::Green => { fmt.color = Color32::from_rgb(0xa4, 0xe4, 0x0); },
						ReprAttr::Blue => { fmt.color = Color32::from_rgb(0x62, 0xd8, 0xf1); },
						ReprAttr::Orange => { fmt.color = Color32::from_rgb(232, 125, 62); },
						ReprAttr::Red => { fmt.color = Color32::RED; },
						ReprAttr::Yellow => { fmt.color = Color32::from_rgb(229, 181, 103); },
						ReprAttr::Purple => { fmt.color = Color32::from_rgb(0xaf, 0x87, 0xff); },
						_ => todo!(),
					}
				}

				match self.expr_idx {
					Some(idx) => {
						let selected_idx = rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][idx];
						let selected_ranges = &rcb.printer.node_ranges[&selected_idx];

						if let Some((start, end)) = selected_ranges.get(&i) {
							if line_len >= *start && (line_len + token.lit.len()) <= *end {
								fmt.background = Color32::from_white_alpha(0x10);
							}
						}
					},
					None => {
						if let Some((blk_idx, start, delta, _)) = &self.selected_range {
							let other = ((*start as i32) + *delta) as usize;
							let min_idx = (*start).min(other);
							let max_idx = (*start).max(other);

							let stack = &rcb.printer.idx_stacks[&(i, line_len)];
							let seq = get_seq(&pb, *blk_idx).unwrap();

							for (i, idx) in stack[1..].iter().enumerate() {
								if idx == blk_idx {
									let seq_idx = seq.iter().position(|&x| x == stack[i]).unwrap();

									if seq_idx >= min_idx && seq_idx <= max_idx {
										fmt.background = Color32::from_white_alpha(0x10);
									}

									break;
								}
							}
						}
						else if (i == self.curr_line && j == self.curr_token) || (
							self.curr_line < rcb.printer.lines.len() &&
							self.curr_token < rcb.printer.lines[self.curr_line].len() &&
							rcb.printer.lines[self.curr_line][self.curr_token].expr.is_some() &&
							token.expr == rcb.printer.lines[self.curr_line][self.curr_token].expr
							) {
							fmt.background = Color32::from_white_alpha(0x10);
						}
					}
				}

				job.append(text.as_str(), 0.0, fmt);

				if token.expr.is_some() {
					line_tokens.push(j);
					line_cols.push(line_len);
				}

				line_len += token.lit.len();
			}

			if i < num_lines - 1 {
				job.append("\n", 0.0, egui::TextFormat::simple(egui::FontId::monospace(9.0), Color32::from_rgb(214, 214, 214)));
			}

			expr_tokens.push(line_tokens);
			cols.push(line_cols);
			skip_chars.push(line_skips);
			call_tokens.push(line_calls);
		}

		(
			job,
			expr_tokens,
			cols,
			skip_chars,
			call_tokens,
			foldable_lines,
		)
	}

	fn display_popover_buttons(
		&mut self,
		_lang: Rc<RefCell<Language>>,
		sidebar_size: &Vec2,
		ui: &mut egui::Ui,
		token_rects: &Vec<Vec<egui::Rect>>,
		call_tokens: &Vec<Vec<Option<(u64, Vec<NodeIndex>, NodeIndex)>>>,
        funcs: &mut Vec<Function>,
	) -> bool {
		let mut rerender = false;

        let func = &funcs[self.function];
        let rc = func.repr_ctx.clone();
        let rcb = (*rc).borrow();

        for (i, line) in rcb.printer.lines.iter().enumerate() {
            for (j, token) in line.iter().enumerate() {
                let tr = &token_rects[i][j];

                if !tr.is_finite() || token.expr.is_none() {
                    continue;
                }

                if let Some((target, params, stmt_idx)) = &call_tokens[i][j] {
                    let rect = Rect::from_min_size(pos2(tr.left() - sidebar_size.x * 2.0, tr.top()), sidebar_size.clone());
                    let response = ui.allocate_rect(rect.clone(), egui::Sense::click());
                    let key = (*target, *stmt_idx);

                    let triangle = if self.expanded_calls.contains_key(&key) {
                        let bottom = pos2(rect.left() + rect.width() / 2.0, rect.top() + 3.0 * rect.height() / 4.0);
                        let left = pos2(rect.left(), rect.top() + rect.height() / 4.0);
                        let right = pos2(rect.right(), rect.top() + rect.height() / 4.0);
                        vec![bottom, left, right]
                    } else {
                        let right = pos2(rect.right(), rect.top() + rect.height() / 2.0);
                        let top = pos2(rect.left(), rect.top() + rect.height() / 4.0);
                        let bottom = pos2(rect.left(), rect.top() + 3.0 * rect.height() / 4.0);
                        vec![top, bottom, right]
                    };

                    ui.painter().add(egui::Shape::convex_polygon(triangle, Color32::DARK_GRAY, PathStroke::NONE));

                    if response.clicked() {
                        if self.expanded_calls.contains_key(&key) {
                            self.expanded_calls.remove(&key);
                        } else {
                            let next_tr = &token_rects[i][j + 1];
                            let x = next_tr.left() + next_tr.width() / 2.0;
                            let y = next_tr.bottom() + 5.0;

                            let win_params = Some((params.clone(), func.pool.clone()));
                            self.new_popover = Some((*target, *stmt_idx, x, y, win_params));
                        }

                        rerender = true;
                    }
                }
            }
        }

		rerender
	}

	fn update_popover_positions(
		&mut self,
		token_rects: &Vec<Vec<egui::Rect>>,
		call_tokens: &Vec<Vec<Option<(u64, Vec<NodeIndex>, NodeIndex)>>>,
        scroll_rect: egui::Rect,
        funcs: &mut Vec<Function>,
	) {
        let func = &funcs[self.function];
        let rc = func.repr_ctx.clone();
        let rcb = (*rc).borrow();

        for (i, line) in rcb.printer.lines.iter().enumerate() {
            for (j, token) in line.iter().enumerate() {
                let tr = &token_rects[i][j];

                if !tr.is_finite() || token.expr.is_none() {
                    continue;
                }

                if let Some((target, _params, stmt_idx)) = &call_tokens[i][j] {
                    let key = (*target, *stmt_idx);

                    if let Some((_, x, y, is_visible)) = self.expanded_calls.get_mut(&key) {
                        let next_tr = &token_rects[i][j + 1];
                        *x = next_tr.left() + next_tr.width() / 2.0;
                        *y = next_tr.bottom() + 5.0;
                        *is_visible = scroll_rect.contains(pos2(*x, *y));
                    }
                }
            }
        }
    }

	fn interact_with_tokens(
		&mut self,
		ctx: &egui::Context,
		ui: &mut egui::Ui,
		token_rects: &Vec<Vec<egui::Rect>>,
		expr_tokens: &Vec<Vec<usize>>,
        funcs: &mut Vec<Function>,
	) {
        let func = &funcs[self.function];
		let rc = func.repr_ctx.clone();
		let rcb = (*rc).borrow();

		for (i, line) in rcb.printer.lines.iter().enumerate() {
			for (j, token) in line.iter().enumerate() {
				let tr = &token_rects[i][j];

				if !tr.is_finite() || token.expr.is_none() {
					continue;
				}

				let response = ui.allocate_rect(*tr, egui::Sense::click());

				if response.clicked() {
					self.curr_line = i;
					self.curr_token = j;
					self.curr_token_idx = 0;
					while expr_tokens[i][self.curr_token_idx] != j {
						self.curr_token_idx += 1;
					}
					self.expr_idx = None;
					self.show_menu = false;
					self.show_options = false;
					self.show_stack = false;
					self.selected_range = None;
					ctx.request_repaint();
				}

				if response.double_clicked() {
					let p = func.pool.clone();
					let pb = (*p).borrow();
					let idx = token.expr.clone().unwrap();
					let node = pb.get(idx);

					if matches!(node.kind, HlilKind::Var(_)) {
						self.cmd_buf.clear();
						self.edited_node = Some(idx);
						self.cmd_buf += "_";
						self.started_editing = false;
						self.selected_range = None;
						ctx.request_repaint();
					}
				}
			}
		}
	}

	fn display_fold_buttons(
		&mut self,
		ui: &mut egui::Ui,
		sidebar_size: &Vec2,
		t: Vec2,
		token_rects: &Vec<Vec<egui::Rect>>,
		foldable_lines: &Vec<(usize, usize)>,
        funcs: &mut Vec<Function>,
	) -> bool {
		let func = &mut funcs[self.function];
		let f = func.folded_exprs.clone();
		let mut fb = (*f).borrow_mut();
		let mut rerender = false;

		for (i, expr_idx) in foldable_lines {
			if fb.contains(expr_idx) {
				// let (rect, response) = ui.allocate_exact_size(sidebar_size.clone(), egui::Sense::click());
				let rect = Rect::from_min_size(pos2(t.x, token_rects[*i - 1][0].top()), sidebar_size.clone());
				let response = ui.allocate_rect(rect.clone(), egui::Sense::click());

				let right = pos2(rect.right(), rect.top() + rect.height() / 2.0);
				let top = pos2(rect.left(), rect.top() + rect.height() / 4.0);
				let bottom = pos2(rect.left(), rect.top() + 3.0 * rect.height() / 4.0);

				let triangle = vec![top, bottom, right];
				ui.painter().add(egui::Shape::convex_polygon(triangle, Color32::DARK_GRAY, PathStroke::NONE));

				if response.clicked() {
					fb.remove(expr_idx);
					rerender = true;
				}
			} else {
				// let (rect, response) = ui.allocate_exact_size(sidebar_size.clone(), egui::Sense::click());
				let rect = Rect::from_min_size(pos2(t.x, token_rects[*i - 1][0].top()), sidebar_size.clone());
				let response = ui.allocate_rect(rect.clone(), egui::Sense::click());

				let bottom = pos2(rect.left() + rect.width() / 2.0, rect.top() + 3.0 * rect.height() / 4.0);
				let left = pos2(rect.left(), rect.top() + rect.height() / 4.0);
				let right = pos2(rect.right(), rect.top() + rect.height() / 4.0);

				let triangle = vec![bottom, left, right];
				ui.painter().add(egui::Shape::convex_polygon(triangle, Color32::DARK_GRAY, PathStroke::NONE));

				if response.clicked() {
					fb.insert(*expr_idx);
					rerender = true;
				}
			}
		}

		rerender
	}

	fn propagate_popovers(
		&mut self,
        infos: &Vec<(Option<NodeIndex>, String, Option<NodeIndex>, Option<(Option<NodeIndex>, String)>)>,
        funcs: &mut Vec<Function>,
	) {
        let mut rerender = false;

        for (_, (ix, _, _, _)) in &self.expanded_calls {
            let (edited_node, cmd_buf, curr_var, committed_var) = &infos[*ix];

            if let Some(var_idx) = edited_node {
                self.edited_node = Some(*var_idx);
                self.expr_idx = None;
                self.cmd_buf = cmd_buf.clone();
            }
            else if let Some(idx) = curr_var {
                let func = &funcs[self.function];
                let rc = func.repr_ctx.clone();
                let rcb = (*rc).borrow();

                for (i_, line_) in rcb.printer.lines.iter().enumerate() {
                    let mut num_tok = 0;
                    for (j_, token_) in line_.iter().enumerate() {
                        if token_.expr == Some(*idx) {
                            self.curr_line = i_;
                            self.curr_token = j_;
                            self.curr_token_idx = num_tok;
                        }
                        if token_.expr.is_some() {
                            num_tok += 1;
                        }
                    }
                }

                self.expr_idx = None;
            }

            if let Some((Some(var_idx), new_name)) = committed_var {
                let func = &mut funcs[self.function];
                func.set_var_name(*var_idx, &new_name);

                if let Some(parent_idx) = func.var_map.get(&var_idx) {
                    self.committed_var = Some((*parent_idx, new_name.clone()));
                }

                rerender = true;
            }
        }

        if rerender {
            let func = &mut funcs[self.function];
            self.rerender(func);
        }
	}

	fn handle_keyboard_input(
		&mut self,
		ctx: &egui::Context,
		is_selected: bool,
		expr_tokens: &Vec<Vec<usize>>,
		lang: Rc<RefCell<Language>>,
        funcs: &mut Vec<Function>,
	) -> bool {
		let reanalyze = false;
		let mut rerender = false;
		let mut handled = true;

		let func = &mut funcs[self.function];
		let num_lines = func.num_lines();

		ctx.input(|input| {
			if !is_selected {
				handled = false;
				return;
			}

			let mut new_text = false;

			for event in &input.events {
				if let egui::Event::Text(t) = event {
					self.cmd_buf += t.as_str();
					handled = true;
					new_text = true;
					break;
				}
			}

			// Only handle enter or escape when editing a text field.
			if let Some(var_idx) = &self.edited_node {
				if input.key_released(Key::Enter) {
					func.set_var_name(*var_idx, &self.cmd_buf);

					if let Some(parent_idx) = func.var_map.get(var_idx) {
						self.committed_var = Some((*parent_idx, self.cmd_buf.clone()));
					}
					self.cmd_buf.clear();
					self.edited_node = None;
					rerender = true;
				} else if input.key_released(Key::Escape) {
					self.cmd_buf.clear();
					self.edited_node = None;
				} else if !self.started_editing && new_text {
					self.started_editing = true;
					self.cmd_buf = self.cmd_buf[1..].to_string();
				}
				handled = false;
				return;
			}

            // if !self.cmd_buf.is_empty() {
            //     println!("{} {}", self.cmd_buf, self.is_template_editor);
            // }

			if self.cmd_buf == "j" {
				if let Some((_, start, delta, num_elems)) = &mut self.selected_range {
					*delta = (*delta + 1).min((*num_elems - 1 - *start) as i32);
				} else {
					self.curr_line = (self.curr_line + 1).min(num_lines - 1);
					self.curr_token_idx = self.curr_token_idx.min(expr_tokens[self.curr_line].len().saturating_sub(1));
				}
				self.expr_idx = None;
				self.show_menu = false;
				self.show_options = false;
			}
			else if self.cmd_buf == "k" {
				if let Some((_, start, delta, _num_elems)) = &mut self.selected_range {
					*delta = (*delta - 1).max(-(*start as i32));
				} else {
					self.curr_line = self.curr_line.saturating_sub(1);
				}
				self.curr_token_idx = self.curr_token_idx.min(expr_tokens[self.curr_line].len().saturating_sub(1));
				self.expr_idx = None;
				self.show_menu = false;
				self.show_options = false;
			}
			else if self.cmd_buf == "l" || self.cmd_buf == "w" {
				self.curr_token_idx = (self.curr_token_idx + 1).min(expr_tokens[self.curr_line].len().saturating_sub(1));
				self.expr_idx = None;
				self.show_menu = false;
				self.show_options = false;
			}
			else if self.cmd_buf == "h" || self.cmd_buf == "b" {
				self.curr_token_idx = self.curr_token_idx.saturating_sub(1);
				self.expr_idx = None;
				self.show_menu = false;
				self.show_options = false;
			}
			else if self.cmd_buf == "u" && self.selected_range.is_none() {
				let rc = func.repr_ctx.clone();
				let rcb = (*rc).borrow();

				self.expr_idx = match self.expr_idx {
					Some(idx) => Some((idx + 1).min(rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)].len() - 1)),
					None => Some(1),
				};
				self.show_menu = false;
				self.show_options = false;
			}
			else if self.cmd_buf == "d" && self.selected_range.is_none() {
				self.expr_idx = match self.expr_idx {
					Some(idx) => if idx == 0 {
						None
					} else {
						Some(idx.saturating_sub(1))
					},
					None => None,
				};
				self.show_menu = false;
				self.show_options = false;
			} else if self.cmd_buf == "zc" {
				let rc = func.repr_ctx.clone();
				let rcb = (*rc).borrow();

				if let Some(idx) = self.expr_idx {
					let selected_idx = rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][idx];
					(*func.folded_exprs.clone()).borrow_mut().insert(selected_idx);
					rerender = true;
				} else if let Some((blk_idx, start, delta, _)) = self.selected_range.take() {
					let other = ((start as i32) + delta) as usize;
					let min_idx = start.min(other);
					let max_idx = start.max(other);

					let seq = get_seq(&(*func.pool).borrow(), blk_idx).unwrap();
					let new_seq_children = seq[min_idx..(max_idx + 1)].to_vec();

					let new_seq = make_seq_ref(func.pool.clone(), new_seq_children);

					let mut blk_children = HashMap::default();
					blk_children.insert("stmts".to_string(), new_seq);
					let new_blk = make_tree_ref(func.pool.clone(), "hlil.Block", blk_children, &*lang, 0);

					let mut new_seq_children = seq[..min_idx].to_vec();
					new_seq_children.push(new_blk);
					new_seq_children.append(&mut seq[(max_idx + 1)..].to_vec());

					let new_seq = make_seq_ref(func.pool.clone(), new_seq_children);
					(*func.pool).borrow_mut().replace(blk_idx, new_seq);
					(*func.folded_exprs).borrow_mut().insert(new_blk);
					rerender = true;
				}
			} else if self.cmd_buf == "zo" {
				let rc = func.repr_ctx.clone();
				let rcb = (*rc).borrow();

				if let Some(selected_idx) = rcb.printer.lines[self.curr_line][self.curr_token].expr {
					(*func.folded_exprs.clone()).borrow_mut().remove(&selected_idx);
					rerender = true;
				}
			} else if self.cmd_buf == "za" {
				(*func.folded_exprs.clone()).borrow_mut().clear();
				rerender = true;
			} else if self.cmd_buf == "V" {
				let rc = func.repr_ctx.clone();
				let rcb = (*rc).borrow();

				let p = func.pool.clone();
				let pb = (*p).borrow();

				let stack = &rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)];

				for (i, idx) in stack.iter().enumerate() {
					if let Some(seq) = get_seq(&pb, *idx) {
						let seq_idx = seq.iter().position(|&x| x == stack[i - 1]).unwrap();
						self.selected_range = Some((*idx, seq_idx, 0, seq.len()));
						self.expr_idx = None;
						break;
					}
				}
			} else if self.cmd_buf == "RD" {
				self.show_rects = !self.show_rects
            } else if self.cmd_buf == "R" {
                if let Some((_node_idx, _elem_idx, _delta, _seq_len)) = self.selected_range.as_ref() {
                    todo!()
                } else {
                    let rc = func.repr_ctx.clone();
                    let rcb = (*rc).borrow();
                    let idx_stack = &rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)];
                    self.template_idx = self.expr_idx.map(|i| idx_stack[i]);
                }
            } else if self.cmd_buf == "y" {
                if self.selected_range.is_some() {
                    self.yank = true
                }
			} else if self.cmd_buf == "gt" {
			} else if self.cmd_buf == "gT" {
			} else if self.cmd_buf == "n" {
				let rc = func.repr_ctx.clone();
				let rcb = (*rc).borrow();
				let token = &rcb.printer.lines[self.curr_line][self.curr_token_idx];

				if let Some(idx) = token.expr {
					let p = func.pool.clone();
					let pb = (*p).borrow();
					let node = pb.get(idx);

					if matches!(node.kind, HlilKind::Var(_)) {
						self.cmd_buf.clear();
						self.cmd_buf += "_";
						self.edited_node = Some(idx);
						self.started_editing = false;
						handled = false;
					}
				}
			} else if self.cmd_buf == "X" {
				self.show_xrefs = !self.show_xrefs;
			} else if input.key_released(Key::Tab) {
				self.show_menu = true;
			} else if input.key_released(Key::Escape) {
				self.selected_range = None;
            } else if self.cmd_buf == "T" && self.is_template_editor {
                let rc = func.repr_ctx.clone();
                let rcb = (*rc).borrow();

                let (node_idx, t) = if let Some((_node_idx, _elem_idx, _delta, _seq_len)) = self.selected_range.as_ref() {
                    todo!()
                } else if let Some(idx) = self.expr_idx {
                    let idx = rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][idx];
                    let ty = (*func.pool.borrow()).get(idx).descriptor.as_ref().unwrap().name().clone();
                    let module = (*lang).borrow().modules.values().find(|m| m.descriptors.contains_key(&ty)).unwrap().name.clone();
                    (idx, format!("{}.{}", module, ty))
                } else if let Some(idx) = rcb.printer.lines[self.curr_line][self.curr_token].expr {
                    let ty = (*func.pool.borrow()).get(idx).descriptor.as_ref().unwrap().name().clone();
                    let module = (*lang).borrow().modules.values().find(|m| m.descriptors.contains_key(&ty)).unwrap().name.clone();
                    (idx, format!("{}.{}", module, ty))
                } else {
                    println!("No expression to templatize");
                    return;
                };

                let name = format!("var_{}", self.vars.len());
                let dt = make_data_type(&mut (*func.pool).borrow_mut(), "".to_string(), 8, &*lang);
                let dt = AttrVal::Node(dt);
                let var = make_var(&mut (*func.pool).borrow_mut(), name.clone(), dt, &*lang);
                self.vars.insert(node_idx, t);
                (*func.pool).borrow_mut().replace(node_idx, var);

                rerender = true;
			} else {
				handled = false;
			}
		});

		if handled {
			self.cmd_buf.clear();
		}

		if reanalyze {
			// self.function = Some(func.reanalyze(lang));
            todo!();
		}

		rerender
	}

	fn display_menu(&mut self, ctx: &egui::Context, _lang: Rc<RefCell<Language>>, funcs: &mut Vec<Function>) -> bool {
		let rect = &self.token_rects[self.curr_line][self.curr_token_idx];
		let rerender = false;

		let frame = egui::Frame::none()
			.stroke(Stroke {
				color: Color32::BLACK,
				width: 1.0,
			})
			.fill(Color32::WHITE)
			.rounding(Rounding::same(8.0))
			.inner_margin(Margin::same(5.0));

		if self.show_menu {
			egui::Area::new("Actions".into())
				.fixed_pos(pos2(rect.right() + 10.0, rect.bottom() + 10.0))
				.order(egui::Order::Foreground)
				.show(ctx, |ui| {
					frame.show(ui, |ui| {
						if ui.button("Show Options").clicked() {
							self.show_menu = false;
							self.show_options = true;
						}

						if ui.button("Show Expr Stack").clicked() {
							self.show_menu = false;
							self.show_stack = true;
						}

						if let Some(curr_idx) = self.get_selected_expr(funcs) {
                            // let func = &funcs[self.function];

                            // if {
                                // let p = func.pool.clone();
                                // let pb = (*p).borrow();
                                // let curr_node = pb.get(curr_idx);
                                // curr_node.descends_from(&"hlil.VarInit".to_string()) ||
                                    // curr_node.descends_from(&"hlil.Assign".to_string())
                            // } {
                                // let p = func.pool.clone();
                                // let mut pb = (*p).borrow_mut();
                                // let curr_node = pb.get(curr_idx);

						// 		if ui.button("Propagate").clicked() {
						// 			// TODO: Use inputs and outputs of the descriptor.
						// 			if let HlilKind::Tree(children) = &curr_node.kind {
						// 				let lhs = children["lhs"];
						// 				let rhs = children["rhs"];

                                        // if let Some(parent_idx) = curr_node.parents.first().cloned() {
                                            // let parent = pb.get(parent_idx);

                                            // if let HlilKind::Seq(elems) = &parent.kind {
                                                // let new_elems = elems.iter().filter(|ix| **ix != curr_node.idx).map(|ix| *ix).collect::<Vec<NodeIndex>>();
                                                // pb.items[parent_idx] = HlilNode {
                                                    // idx: parent_idx,
                                                    // kind: HlilKind::Seq(new_elems),
                                                    // addr: parent.addr.clone(),
                                                    // attrs: parent.attrs.clone(),
                                                    // name: parent.name.clone(),
                                                    // parents: parent.parents.clone(),
                                                    // descriptor: parent.descriptor.clone(),
                                                // };
                                            // }
                                        // }

						// 				pb.replace(lhs, rhs);
						// 				rerender = true;
						// 			}
						// 			self.show_menu = false;
						// 		}
						// 	}
                            // else if {
                                // let p = func.pool.clone();
                                // let pb = (*p).borrow();
                                // let curr_node = pb.get(curr_idx);
                                // curr_node.descends_from(&"hlil.Call".to_string())
                            // } {
                                // let mut info = None;

                                // if ui.button("Inline").clicked() {
                                    // if let Some(children) = get_tree_ref(func.pool.clone(), curr_idx) {
                                        // let p = func.pool.clone();
                                        // let pb = (*p).borrow();
                                        // let curr_node = pb.get(curr_idx);

                                        // let addr = pb.items[children["target"]].references(p.clone())[0];
                                        // let json = &self.addr2func[&addr];

                                        // let params = get_seq(&pb, children["params"]).unwrap();
                                        // let func = Function::new(addr, json, lang.clone(), Some((params, p.clone())));

                                        // let fp = func.pool.clone();
                                        // let fpb = fp.borrow();
                                        // let inlined_idx = match &fpb.get(func.root).kind {
                                            // HlilKind::Tree(t) => t["body"],
                                            // _ => panic!(),
                                        // };

                                        // info = Some((curr_node.idx, inlined_idx, fp.clone()));
                                    // }
                                // }

                                // if let Some((old_idx, inlined_idx, old_pool)) = info {
                                    // let mut var_map = HashMap::new();
                                    // let new_idx = deep_copy_ref(func.pool.clone(), inlined_idx, old_pool.clone(), &mut var_map, false);

                                    // let p = func.pool.clone();
                                    // let mut pb = (*p).borrow_mut();
                                    // pb.replace(old_idx, new_idx);

                                    // self.show_menu = false;
                                    // rerender = true;
                                // }
                            // }

                            if ui.button("Create Rule").clicked() {
                                self.show_menu = false;
                                self.template_idx = Some(curr_idx);
                            }
						}
					});
				});
		} else if self.show_options {
            self.show_options = false;
            // let func = &funcs[self.function];
			// let rc = func.repr_ctx.clone();
			// let rcb = (*rc).borrow();

			// let node_idx = match self.expr_idx {
				// Some(idx) => {
					// rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][idx]
				// },
				// None => {
					// rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)][0]
				// }
			// };

			// let (start_idx, end_idx) = rcb.printer.node_to_raw_range[&node_idx];
			// let tmpl_ctx = Rc::new(RefCell::new(TemplateContext { repr_ctx: func.repr_ctx.clone() }));
			// let options = get_options(start_idx, end_idx, func.pool.clone(), tmpl_ctx.clone(), lang.clone());

            // let p = func.pool.clone();
            // let mut pb = (*p).borrow_mut();

			// egui::Area::new("Actions".into())
				// .fixed_pos(pos2(rect.right() + 10.0, rect.bottom() + 10.0))
				// .order(egui::Order::Foreground)
				// .show(ctx, |ui| {
					// frame.show(ui, |ui| {
						// for (node_type, node_children) in &options {
							// if ui.button(*node_type).clicked() {
								// self.show_options = false;

								// let l = lang.clone();
								// let desc = (*l).descriptor_for(node_type);
								// let c = (*tmpl_ctx).borrow();
								// let old_idxs = &(*c.repr_ctx).borrow().printer.raw_range_to_node[&(start_idx, end_idx)];

								// for old_idx in old_idxs {
									// println!("{} {}", desc.superclass, (*func.pool).borrow().get(*old_idx).descriptor.as_ref().unwrap().name());
									// let old_node = pb.get(*old_idx);

									// if old_node.descends_from(&desc.superclass) {
										// println!("replacing");
										// let new_idx = make_tree_ref(func.pool.clone(), node_type, node_children.clone(), &*lang, old_node.addr.unwrap());
										// pb.replace(*old_idx, new_idx);

										// // let repr_ctx = Rc::new(RefCell::new(ReprContext::new()));
										// // println!("{}", (*func.pool).borrow().get(new_idx).to_string(func.pool.clone(), repr_ctx.clone(), func.folded_exprs.clone()));
										// (*func.repr_ctx).borrow_mut().printer.reset();
										// break;
									// }
								// }
							// }
						// }
					// });
				// });
		} else if self.show_stack {
            let func = &funcs[self.function];
			let rc = func.repr_ctx.clone();
			let rcb = (*rc).borrow();

			for idx in &rcb.printer.idx_stacks[&(self.curr_line, self.curr_col)] {
				println!("{}\n-------------", (*func.pool).borrow().get(*idx).str(func.pool.clone()));
			}
			self.show_stack = false;
		}

		rerender
	}

    pub fn display_decomp(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        lang: Rc<RefCell<Language>>,
        is_selected: bool,
        funcs: &mut Vec<Function>,
    ) {
		let frame = egui::Frame::none()
			.inner_margin(Margin::same(2.0))
			.fill(Color32::from_rgb(46, 46, 46));

		frame.show(ui, |ui| {
			// HACK: First calculate the size of a label to get the dimensions of the sidebar.
			let mut rerender = false;
			let sidebar_size = calculate_sidebar_size(ctx);

			let mut token_rects = vec![];
			let mut expr_tokens = vec![];
			let mut call_tokens = vec![];
			let mut cols = vec![];

			if self.show_xrefs {
				egui::SidePanel::left("x-refs").frame(frame).show_inside(ui, |ui| {
					egui::ScrollArea::vertical().show(ui, |ui| {
						for (func_addr, call_addr) in funcs[self.function].xrefs.iter() {
							if ui.label(format!("0x{:x}", call_addr)).clicked() {
								self.new_parent = Some((*func_addr, *call_addr));
							}
						}
					});
				});
			}

			let scroll_state = egui::ScrollArea::both()
				.id_source(format!("{}_decomp", funcs[self.function].addr))
				.animated(false)
				.show(ui, |ui| {
				let (
					job,
					_expr_tokens,
					_cols,
					skip_chars,
					_call_tokens,
					foldable_lines,
				) = self.format_text(
					&sidebar_size,
                    funcs,
				);

				expr_tokens = _expr_tokens;
				call_tokens = _call_tokens;
				cols = _cols;

				let cursor = ui.cursor();
				let t = vec2(cursor.left(), cursor.top());

				token_rects = calculate_rects(
					&funcs[self.function],
					self.edited_node.clone(),
					self.cmd_buf.len(),
					ctx,
					job.clone(),
					t.clone(),
					skip_chars,
				);

                let label = egui::Label::new(job).extend();
				ui.add(label);

				if self.show_rects {
					for row in &token_rects {
						for rect in row {
							ui.painter().add(
                                egui::Shape::rect_stroke(
                                    *rect,
                                    Rounding::ZERO,
                                    egui::Stroke::new(1.0, Color32::RED),
                                )
                            );
						}
					}
				}

				rerender |= self.display_popover_buttons(
					lang.clone(),
					&sidebar_size,
					ui,
					&token_rects,
					&call_tokens,
                    funcs,
				);

				self.interact_with_tokens(
					ctx,
					ui,
					&token_rects,
					&expr_tokens,
                    funcs,
				);

				rerender |= self.display_fold_buttons(
					ui,
					&sidebar_size,
					t.clone(),
					&token_rects,
					&foldable_lines,
                    funcs,
				);
			});

            let r = &token_rects[self.curr_line][0];
            let min_ = pos2(0.0, r.top());
            let max_ = pos2(scroll_state.inner_rect.right(), r.bottom());
            let line_rect = Rect::from_min_max(min_, max_);

            ui.painter().add(
                egui::Shape::rect_filled(
                    line_rect,
                    Rounding::ZERO,
                    Color32::from_white_alpha(0x10),
                )
            );

            self.update_popover_positions(
                &token_rects,
                &call_tokens,
                scroll_state.inner_rect,
                funcs,
            );

			self.token_rects = token_rects;

			rerender |= self.handle_keyboard_input(ctx, is_selected, &expr_tokens, lang.clone(), funcs);
			rerender |= self.display_menu(ctx, lang.clone(), funcs);

			if ui.input(|input| input.pointer.any_click()) && ui.ui_contains_pointer() {
				self.was_selected = true;
			}

			if rerender {
				self.rerender(&mut funcs[self.function]);
				return;
			}

			self.curr_token_idx = self.curr_token_idx.min(expr_tokens[self.curr_line].len().saturating_sub(1));

			if self.curr_token_idx < expr_tokens[self.curr_line].len() {
				self.curr_token = expr_tokens[self.curr_line][self.curr_token_idx];
				self.curr_col = cols[self.curr_line][self.curr_token_idx];
			}

            if self.is_template_editor {
                let text_edit = egui::TextEdit::multiline(&mut self.rule_repr);
                ui.add(text_edit);

                if ui.button("Save").clicked() {
                    let mut child_types = HashMap::new();

                    for (v, t) in &self.vars {
                        let kind = (*funcs[self.function].pool).borrow().get(*v).kind.clone();

                        if let HlilKind::Var(name) = kind {
                            (&mut funcs[self.function]).set_var_name(*v, &format!("${}", name));
                            child_types.insert(name, t.clone());
                        }
                    }

                    let ty = (*funcs[self.function].pool).borrow().get(funcs[self.function].root).descriptor.as_ref().unwrap().name().clone();
                    let module_name = (*lang).borrow().modules.values().find(|m| m.descriptors.contains_key(&ty)).unwrap().name.clone();
                    let full_type = format!("{}.{}", module_name, ty);

                    let mut rule_str = String::new();
                    rule_str += format!("import ideco\n\n").as_str();
                    rule_str += format!("class Rule({}):\n", full_type).as_str();

                    for (name, ty) in child_types {
                        rule_str += format!("    {}: {}\n", name, ty).as_str();
                    }

                    let template = funcs[self.function].raw_string();

                    rule_str += format!("    \n").as_str();
                    rule_str += format!("    @staticmethod\n").as_str();
                    rule_str += format!("    def match():\n").as_str();
                    rule_str += format!("        import ideco\n").as_str();
                    rule_str += format!("        return ideco.eval_tmpl(\"{}\")[0]\n", template).as_str();
                    rule_str += format!("    \n").as_str();
                    rule_str += format!("    def to_string(self):\n").as_str();
                    rule_str += format!("        return ideco.eval_repr(\"{}\")\n", self.rule_repr).as_str();

                    println!("{}", rule_str);

                    Python::with_gil(|py| {
                        let sys = py.import("sys").unwrap();

                        let path = sys.getattr("path").unwrap();
                        let path_list = path.downcast::<PyList>().unwrap();

                        let current_dir = std::env::current_dir().unwrap();
                        let current_dir_str = current_dir.to_string_lossy().to_string();
                        path_list.append(current_dir_str).unwrap();
                        path_list.insert(0, current_dir.join("languages").to_string_lossy().to_string()).unwrap();

                        let hlil = py.import("hlil").unwrap();

                        let mod_name = "tmp_module".to_string();
                        // let py_module = PyModule::new(py, &mod_name).unwrap();
                        // let py_module = py.import(mod_name.as_str()).unwrap();
                        let py_module = sys.getattr("modules").unwrap().get_item(&mod_name).unwrap().downcast::<PyModule>().unwrap();

                        let globals = PyDict::new(py);
                        let _ = globals.set_item("hlil", hlil);
                        let _ = py.run(&rule_str, Some(globals), Some(py_module.dict())).unwrap();
                        let py_desc: Py<PyAny> = py_module.getattr("Rule").unwrap().into();
                        let desc = NodeDescriptor::new(py_desc.bind(py).clone());
                        // lang.borrow_mut().modules.get_mut(&mod_name).unwrap().add_descriptor(desc.name().to_string(), &desc);

                        let mut descs = HashMap::new();
                        descs.insert("Rule".to_string(), desc);
                        lang.borrow_mut().modules.insert(mod_name.clone(), LanguageModule {
                            name: mod_name,
                            descriptors: descs,
                        });
                    });

                    self.lift(&mut funcs[self.function], lang.clone());
                }
            }
		});
    }
}

struct DecompilationWidget<'a> {
    ctx: &'a egui::Context,
    win: DecompilationWindow,
    lang: Rc<RefCell<Language>>,
    is_selected: bool,
    funcs: &'a mut Vec<Function>,
}

impl<'a> egui::widgets::Widget for DecompilationWidget<'a> {
    fn ui(mut self, ui: &mut egui::Ui) -> egui::Response {
        self.win.display_decomp(self.ctx, ui, self.lang.clone(), self.is_selected, self.funcs);
        ui.interact(Rect::NOTHING, egui::Id::new("foobar"), egui::Sense::click())
    }
}

fn insert_whitespace(
    job: &mut egui::text::LayoutJob,
    p1: &markdown::unist::Point,
    p2: &markdown::unist::Point,
    fmt: &egui::TextFormat,
) {
    // println!("{:?}, {:?}", p1, p2);
    job.append(&"\n".repeat(p2.line - p1.line), 0.0, fmt.clone());
    job.append(&" ".repeat(p2.column.saturating_sub(p1.column)), 0.0, fmt.clone());
}

pub fn render_markdown(
    job: &mut egui::text::LayoutJob,
    md: &mdnode,
    fmt: &egui::TextFormat,
    cursor: &mut Option<markdown::unist::Point>,
    snippets: &mut Vec<((usize, usize), (usize, usize), FunctionIndex, NodeIndex)>,
) {
    match md {
        mdnode::Root(root) => {
            for (_, child) in root.children.iter().enumerate() {
                render_markdown(job, child, fmt, cursor, snippets);
            }
        },
        mdnode::Paragraph(p) => {
            for (_, child) in p.children.iter().enumerate() {
                render_markdown(job, child, fmt, cursor, snippets);
            }
        },
        mdnode::Heading(h) => {
            let mut fmt = fmt.clone();
            fmt.font_id = egui::FontId::monospace(12.0);

            job.append("#", 0.0, fmt.clone());
            *cursor = md.position().cloned().map(|p| p.start);

            if let Some(pos) = cursor.as_mut() {
                pos.column += 1;
                pos.offset += 1;
            }

            for (_, child) in h.children.iter().enumerate() {
                render_markdown(job, child, &fmt, cursor, snippets);
            }
        },
        mdnode::Text(t) => {
            if let (Some(p1), Some(p2)) = (&cursor, md.position()) {
                insert_whitespace(job, &p1, &p2.start, fmt);
            }

            *cursor = md.position().cloned().map(|p| p.end);
            job.append(&t.value, 0.0, fmt.clone());
        },
        mdnode::Html(html) => {
            if let (Some(p1), Some(p2)) = (&cursor, md.position()) {
                insert_whitespace(job, &p1, &p2.start, fmt);
            }

            *cursor = md.position().cloned().map(|p| p.end);
            job.append(&html.value, 0.0, fmt.clone());
        },
        mdnode::Code(code) => {
            if let (Some(p1), Some(p2)) = (&cursor, md.position()) {
                insert_whitespace(job, &p1, &p2.start, fmt);
            }

            let mut fmt = fmt.clone();
            fmt.font_id = egui::FontId::monospace(9.0);

            job.append("```", 0.0, fmt.clone());

            if let Some(lang) = &code.lang {
                job.append(format!("{}", lang).as_str(), 0.0, fmt.clone());
            }

            if let Some(meta) = &code.meta {
                job.append(format!(" {}", meta).as_str(), 0.0, fmt.clone());
            }

            job.append("\n", 0.0, fmt.clone());
            job.append(&code.value, 0.0, fmt.clone());
            job.append("\n```", 0.0, fmt.clone());

            if let (Some(pos), Some(meta)) = (md.position(), &code.meta) {
                let info_str = meta.split("=").nth(1).unwrap();
                let comps: Vec<&str> = info_str[1..info_str.len() - 1].split("_").collect();
                let func_idx = comps[0].parse::<FunctionIndex>().unwrap();
                let node_idx = comps[1].parse::<NodeIndex>().unwrap();
                let start = (pos.start.line - 1, pos.start.column - 1);
                let end = (pos.end.line - 1, pos.end.column - 2);
                snippets.push((start, end, func_idx, node_idx));
            }

            *cursor = md.position().cloned().map(|p| p.end);
        },
        mdnode::Break(_) => {
            if let (Some(p1), Some(p2)) = (&cursor, md.position()) {
                insert_whitespace(job, &p1, &p2.start, fmt);
            }

            let mut fmt = fmt.clone();
            fmt.font_id = egui::FontId::monospace(9.0);

            job.append("\n", 0.0, fmt.clone());
            *cursor = md.position().cloned().map(|p| p.end);
        },
        _ => todo!("{:?}", md),
    }
}

#[allow(deprecated)]
impl NotesWindow {
    pub fn display(
        &mut self,
        ctx: &egui::Context,
        ui: &mut egui::Ui,
        lang: Rc<RefCell<Language>>,
        _is_selected: bool,
        workspace_name: &String,
        paste_buffer: Option<(FunctionIndex, NodeIndex, usize, i32, usize)>,
        funcs: &mut Vec<Function>,
    ) {
        let mut snippets = vec![];
        let mut foo = HashMap::new();
        let size = ui.available_size();

        fs::write(format!("notes/{}.md", workspace_name), self.text.as_str()).unwrap();

        egui::ScrollArea::both().show(ui, |ui| {
        let mut layouter = |ui: &egui::Ui, string: &str, _wrap_width: f32| {
            let mut job = egui::text::LayoutJob::default();
            job.wrap.max_width = f32::INFINITY;

            let md = markdown::to_mdast(string, &markdown::ParseOptions::default()).unwrap();
            let fmt = egui::TextFormat::simple(egui::FontId::monospace(9.0), Color32::from_rgb(214, 214, 214));

            let mut last_pos = None;
            render_markdown(&mut job, &md, &fmt, &mut last_pos, &mut snippets);

            if let Some(p) = last_pos {
                job.append(&string[p.offset..], 0.0, fmt.clone());
            }

            // println!("{}", string);
            // println!("{}", job.text);

            ui.fonts(|fonts| {
                let galley = fonts.layout_job(job);

                for ((start_row, start_col), (end_row, end_col), func_idx, node_idx) in &snippets {
                    // println!("{} {} {} {}", start_row, start_col, end_row, end_col);
                    let mut rect = galley.rows[*start_row].glyphs[*start_col].logical_rect();
                    rect = rect.union(galley.rows[*end_row].glyphs[*end_col].logical_rect());
                    rect.extend_with_x(size.x);

                    foo
                        .entry((*func_idx, *node_idx))
                        .or_insert_with(Vec::new)
                        .push((rect.top(), rect.left(), rect.bottom(), rect.right()));
                }

                galley
            })
        };

        let text_edit = egui::TextEdit::multiline(&mut self.text)
            .frame(false)
            .layouter(&mut layouter);
            // .interactive(self.mode == EditMode::Insert);

        let cursor = ui.cursor();
        let trans = vec2(cursor.left(), cursor.top());

        if ui.add_sized(ui.available_size(), text_edit).secondary_clicked() {
            self.menu_pos = ui.input(|i| i.pointer.interact_pos());
        }

        for ((func_idx, _node_idx), rects) in &foo {
            for (t, l, b, r) in rects {
                let rect = Rect::from_min_max(pos2(*l, *t), pos2(*r, *b)).translate(trans.clone());
                let mut win = DecompilationWindow::default();
                win.set_addr(*func_idx);
                // win.addr2func = self.addr2func.clone();

                let widget = DecompilationWidget {
                    ctx: ctx,
                    win: win,
                    lang: lang.clone(),
                    is_selected: false,
                    funcs: funcs,
                };

                // ui.put(rect, widget);
                ui.allocate_ui_at_rect(rect, |ui| {
                    ui.vertical(|ui| {
                        ui.add(widget);
                    });
                });
            }
        }

        if let Some(pos) = &self.menu_pos {
            egui::popup::show_tooltip_at(ctx, ui.layer_id(), egui::Id::new("menu"), *pos, |ui| {
                if ui.button("Paste").clicked() {
                    if let Some((func_idx, seq_idx, start, delta, _)) = paste_buffer {
                        let func = &funcs[func_idx];

                        let p = func.pool.clone();
                        let pb = (*p).borrow();

                        let rc = func.repr_ctx.clone();
                        let rcb = (*rc).borrow();

                        if let Some(seq) = get_seq(&pb, seq_idx) {
                            let other = ((start as i32) + delta) as usize;
                            let min_idx = start.min(other);
                            let max_idx = start.max(other);

                            let mut start = 10000;
                            let mut end = 0;

                            for ix in min_idx..=max_idx {
                                let (s, e) = &rcb.printer.node_to_raw_range[&seq[ix]];
                                start = start.min(*s);
                                end = end.max(*e);
                            }

                            self.text += format!("```Objective-C info=\"{}_{}\"\n", func_idx, min_idx).as_str();
                            self.text += &rcb.printer.raw_text[start..end];
                            self.text += "\n```\n";
                            self.pasted = true;
                        }
                    }

                    self.menu_pos = None;
                }
            });
        }
        });

        if ui.input(|input| input.pointer.any_click()) && ui.ui_contains_pointer() {
            self.was_selected = true;
        }
    }
}

pub fn run_gui(args: Args) {
	let _ = eframe::run_native(
        "ideco",
        eframe::NativeOptions {
            viewport: egui::ViewportBuilder::default().with_inner_size([900.0, 600.0]),
            ..Default::default()
        },
        Box::new(|cc| {
            let mut fonts = egui::FontDefinitions::default();

            fonts.font_data.insert(
                "SourceCodePro".to_string(),
                egui::FontData::from_static(include_bytes!(
                    "/Users/samlerner/Library/Fonts/SourceCodePro-Regular.otf"
                )),
            );

            fonts.font_data.insert(
                "SourceCodePro-Bold".to_string(),
                egui::FontData::from_static(include_bytes!(
                    "/Users/samlerner/Library/Fonts/SourceCodePro-Bold.otf"
                )),
            );

            fonts.families.entry(FontFamily::Monospace).or_default().insert(0, "SourceCodePro".to_string());
            fonts.families.entry(FontFamily::Name("SCP-Bold".into())).or_default().insert(0, "SourceCodePro-Bold".to_string());

            cc.egui_ctx.set_fonts(fonts);
            cc.egui_ctx.set_visuals(egui::Visuals::light());

            // Ok(Box::<App>::default())
			// let app = if let Some(storage) = cc.storage {
			// 	if let Some(app) = eframe::get_value(storage, eframe::APP_KEY) {
			// 		app
			// 	} else {
			// 		App::new(args.clone())
			// 	}
			// } else {
			// 	App::new(args.clone())
			// };

            let app = App::new(args.clone());
			Ok(Box::new(app))
        })
    );
}
