use ast::File;
use inkwell::{
    context::Context,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};
use lexer::Lexer;

use crate::{
    ast::{Function, FunctionDecl, Ident, Span},
    complier::Compiler,
};
use std::{io::Write, path::Path};

mod ast;
mod complier;
mod lexer;
mod tokens;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] #[allow(dead_code)] pub kaleidoscope);

macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

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
    // let input = "def foo(a b) a*a + 2*a*b + b*b;foo(1, 2);";
    // eq = (a+b)*(a+b)
    // let input = "if 1>1 then 2 else 5 end;";
    // let input = "
    //     extern putchard(char);
    // def printstar(n)
    //   for i = 1, i < n, 1.0 in
    //     putchard(42);
    //   end;
    // printstar(100);";

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

    println!("---------llvm print---------");
    result.print_to_string(&complier);
    println!("---------llvm call---------");
    run_target(&result, &complier);
    gen_target(&complier);
}

fn gen_target(complier: &Compiler) {
    let config = InitializationConfig {
        asm_parser: true,
        asm_printer: true,
        base: true,
        disassembler: false,
        info: true,
        machine_code: true,
    };
    Target::initialize_all(&config);
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).unwrap();
    let tm = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    let dl = tm.get_target_data().get_data_layout();
    complier.module.set_data_layout(&dl);
    complier.module.set_triple(&target_triple);
    let path = Path::new("kaleidoscope.o");
    tm.write_to_file(&complier.module, FileType::Object, &path)
        .unwrap();
}

fn run_target(result: &File, complier: &Compiler) {
    let execution_engine = complier
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    for item in result.items.clone() {
        match item {
            ast::Item::Extern(e) => {
                complier.compile_fn_decl(&e).unwrap();
            }
            ast::Item::Function(f) => {
                complier.compile_fn(&f).unwrap();
            }
            ast::Item::Expr(e) => {
                let name = "top".to_owned();
                let func = Function {
                    decl: FunctionDecl {
                        ident: Ident {
                            name: name.clone(),
                            span: Span::initial(),
                        },
                        args: vec![],
                        span: Span::initial(),
                    },
                    body: Some(e),
                    span: Span::initial(),
                    is_anon: true,
                };
                let _ = complier.compile_fn(&func).unwrap();
                let maybe_fn = unsafe {
                    execution_engine
                        .get_function::<unsafe extern "C" fn() -> f64>(name.as_str())
                        .unwrap()
                };
                let out = unsafe { maybe_fn.call() };
                println!("out: {}", out)
            }
        }
    }
}
