use inkwell::{context::Context, passes::PassManager};
use lexer::Lexer;

use crate::complier::Compiler;

mod ast;
mod complier;
mod lexer;
mod tokens;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] #[allow(dead_code)] pub kaleidoscope);

fn main() {
    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    // let input = "def foo(a b) 5 + 3.15";
    // let input = "def foo(a b) a*a + 2*a*b + b*b;";
    // let input = "5 + 2;";
    // let input = "def foo(a b) a*a + 2*a*b + b*b;def bar(a) foo(a, 4.0) + bar(31337);";
    // let input = "extern cos(x);cos(1.234);";
    // let input = "def test(x) 1+2+x;";
    let input = "def test(x) (1+2+x)*(x+(1+2));";

    let lexer = Lexer::new(input);

    println!("--------lexer--------");
    for ele in lexer {
        println!("{:?}", ele);
    }

    let lexer = Lexer::new(input);

    let parser = kaleidoscope::FileParser::new();

    let result = parser.parse(lexer).unwrap();
    println!("--------parser--------");
    println!("{:?}", result);

    let complier = Compiler::new(&context, &builder, &fpm, &module);

    println!("---------llvm---------");
    result.print_to_string(&complier);
}
