use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, PointerValue,
};
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{Expr, Function, FunctionDecl, Operator};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    variables: RefCell<HashMap<String, PointerValue<'ctx>>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            builder,
            fpm,
            module,
            variables: RefCell::new(HashMap::new()),
        }
    }
}
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile_expr(&self, expr: &Expr) -> Result<FloatValue<'ctx>, &'static str> {
        match expr {
            Expr::Ident(id) => match self.variables.borrow().get(&id.name) {
                Some(var) => self
                    .builder
                    .build_load(self.context.f64_type(), *var, &id.name)
                    .map(|res| res.into_float_value())
                    .map_err(|_| "load fail"),
                None => Err("Could not find a matching variable."),
            },
            Expr::Literal(v) => Ok(self.context.f64_type().const_float(v.value)),
            Expr::FunctionCall(fc) => {
                let callee_f = self
                    .module
                    .get_function(&fc.ident.name)
                    .ok_or("Could not find a matching function.")?;
                if callee_f.count_params() != fc.args.len() as u32 {
                    return Err("Incorrect # arguments passed");
                }
                let mut args: Vec<BasicMetadataValueEnum> = vec![];
                for arg in fc.args.iter() {
                    args.push(self.compile_expr(&arg)?.into());
                }
                match self.builder.build_call(callee_f, args.as_slice(), "tmp") {
                    Ok(res) => Ok(res
                        .try_as_basic_value()
                        .left()
                        .map(|x| x.into_float_value())
                        .ok_or("fail call result to basic value")?),
                    Err(_err) => Err("build error"),
                }
            }
            Expr::Assign { ident, value } => match self.variables.borrow_mut().insert(
                ident.name.clone(),
                self.compile_expr(value)?
                    .as_basic_value_enum()
                    .into_pointer_value(),
            ) {
                Some(_) => Ok(self.context.f64_type().const_float(1.)),
                None => Err("assign var error"),
            },
            Expr::BinOp { left, op, right } => {
                let l = self.compile_expr(&left)?;
                let r = self.compile_expr(&right)?;
                match op {
                    Operator::Add => match self.builder.build_float_add(l, r, "tmpadd") {
                        Ok(v) => Ok(v),
                        Err(err) => {
                            println!("{}", err.to_string());
                            Err("build add error")
                        }
                    },
                    Operator::Sub => match self.builder.build_float_sub(l, r, "tmpsub") {
                        Ok(v) => Ok(v),
                        Err(_err) => Err("build sub error"),
                    },
                    Operator::Mult => match self.builder.build_float_mul(l, r, "tmpmul") {
                        Ok(v) => Ok(v),
                        Err(_err) => Err("build mul error"),
                    },
                    Operator::Div => match self.builder.build_float_div(l, r, "tmpdiv") {
                        Ok(v) => Ok(v),
                        Err(_err) => Err("build div error"),
                    },
                    Operator::Mod => todo!(),
                    // Operator::Mod => match self.builder.build_float_add(l, r, "tmpadd") {
                    //     Ok(v) => Ok(v),
                    //     Err(_err) => Err("build error"),
                    // },
                }
            }
            Expr::UnaryOp { operand: _, op: _ } => todo!(),
        }
    }
    pub fn compile_fn_decl(
        &self,
        decl: &FunctionDecl,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(decl.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self
            .module
            .add_function(decl.ident.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(&decl.args[i].name);
        }

        // finally return built prototype
        Ok(fn_val)
    }
    pub fn compile_fn(&self, expr: &Function) -> Result<FunctionValue<'ctx>, &'static str> {
        let func = if let Some(f) = self.module.get_function(&expr.decl.ident.name) {
            f
        } else {
            self.compile_fn_decl(&expr.decl)?
        };
        if expr.body.is_none() {
            return Ok(func);
        }
        let bb = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(bb);
        self.variables.borrow_mut().reserve(expr.decl.args.len());

        let builder = self.context.create_builder();

        for (i, param) in func.get_param_iter().enumerate() {
            let arg_name = expr.decl.args[i].name.as_str();

            let entry = func.get_first_basic_block().unwrap();
            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(entry),
            }
            let alloca = builder
                .build_alloca(self.context.f64_type(), arg_name)
                .unwrap();

            self.builder.build_store(alloca, param).unwrap();
            self.variables
                .borrow_mut()
                .insert(arg_name.to_owned(), alloca);
        }
        let body = self.compile_expr(expr.body.as_ref().unwrap())?;
        self.builder.build_return(Some(&body)).unwrap();

        if func.verify(true) {
            self.fpm.run_on(&func);
            Ok(func)
        } else {
            unsafe {
                func.delete();
            }
            Err("Invalid generated function.")
        }
    }
}
