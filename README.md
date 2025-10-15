# ideco

Code for the [ideco](/paper.pdf) paper.
This code is still under heavy development and is experimental so it's not suitable for production use.
The DSLs/interfaces are likely to undergo large changes in the future.

## Description

*ideco* is an experiment in how to generically decompile non-C languages (C++, Rust, Swift, etc.).
It does this by defining and applying rewrite rules to the concrete syntax tree (CST).
These rules are grouped into modules in the `languages` folder.
The existing language modules are:

* hlil: The initial descriptors that the HLIL tree is mapped to.
* raii: Matches destructor calls (currently limited in scope).
* rc: Matches reference-counting calls (current limited in scope).
* values: Deals with value types such as structs on the stack or in multiple registers.
* canaries: Matches stack canary code.
* control_flow: Miscellaneous control flow simplifications.
* swift: Swift specific syntax rules.

## Usage

The code currently operates on a json dump of the HLIL tree from Binary Ninja.
To generate this dump, run the `dump_hlil.py` script in Binary Ninja.
The main function needs to be called with the current binary view and an file path for the json file.

In the project root run `cargo run [--release]` with the following arguments:

* --path/-p: Path to the exported json file.
* --addr/-a: Address of the function to process.
* --modules/-m: Pass for each module to use (e.g. `-m values -m swift`).
* --gui/-g: Optional if the function should be displayed in the gui after processing (experimental).

To test new rules, simply edit or create a new module in the `languages` folder with your new rules and pass tell ideco to use it with the `-m` argument.

Currently the gui doesn't do much more than display the code but there are plans to increase its capabilities.
For a full list of the keyboard mappings, look at `DecompilationWindow::handle_keyboard_input`.

## Gotchas

Rules are applied in an semi-unordered manner.
Therefore, sometimes the ordering will cause the rest of the matching to fail or crash.
In this case, just re-run ideco with the same arguments.
This will hopefully be fixed soon.
