mod log;
mod hlil;
mod pool;
mod lang;
mod repr;
mod expr;
mod template;
mod function;
mod windows;
mod workspace;
mod app;
mod gui;
mod serialization;

use gui::run_gui;
use app::App;

use clap::{self, Parser};

use std::path::PathBuf;

fn parse_hex(src: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}

#[derive(Clone, Parser)]
pub struct Args {
    #[arg(short = 'm', long = "modules", num_args = 0..)]
    modules: Vec<String>,

    #[arg(short = 'g', long = "gui")]
    gui: bool,

    #[arg(short = 'p', long = "path")]
    path: PathBuf,

    #[arg(short = 'a', long = "addr")]
    #[clap(value_parser = parse_hex)]
    addr: u64,
}

fn main() {
    let args = Args::parse();
    let addr = args.addr;

    if args.gui {
        run_gui(args);
    } else {
        let mut app = App::new(args);
        let ws = &mut app.workspaces[0];
        let _ = ws.open_function(addr, 0.0, 0.0, 0.0, 0.0, app.lang.clone(), None, true, true);
        let func = &ws.functions[0];
        println!("{}", func.rerender());
    }
}
