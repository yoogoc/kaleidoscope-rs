use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, PointerValue,
};
use inkwell::FloatPredicate;
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
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.builder.get_insert_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }
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
            Expr::BinOp {
                left,
                op,
                right,
                span: _,
            } => {
                let l = self.compile_expr(&left)?;
                let r = self.compile_expr(&right)?;
                match op {
                    Operator::Add => match self.builder.build_float_add(l, r, "tmpadd") {
                        Ok(v) => Ok(v),
                        Err(_err) => Err("build add error"),
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
                    _ => unreachable!(),
                }
            }
            Expr::UnaryOp {
                operand: _,
                op: _,
                span: _,
            } => todo!(),
            Expr::BoolOp {
                left,
                op,
                right,
                span: _,
            } => {
                let l = self.compile_expr(&left)?;
                let r = self.compile_expr(&right)?;
                let op = match op {
                    Operator::More => FloatPredicate::OGT,
                    Operator::Less => FloatPredicate::OLT,
                    Operator::MoreEq => FloatPredicate::OGE,
                    Operator::LessEq => FloatPredicate::OLE,
                    Operator::Equal => FloatPredicate::OEQ,
                    _ => unreachable!(),
                };
                let cmp = self
                    .builder
                    .build_float_compare(op, l, r, "tmpcmp")
                    .map_err(|_err| "build_float_compare")?;
                Ok(self
                    .builder
                    .build_signed_int_to_float(cmp, self.context.f64_type(), "tmpbool")
                    .map_err(|_err| "build_signed_int_to_float")?)
            }
            Expr::If {
                cond,
                truth,
                falsity,
                span: _,
            } => {
                let cond = self.compile_expr(cond)?;
                let condv = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        cond,
                        self.context.f64_type().const_float(0.),
                        "ifcond",
                    )
                    .map_err(|_err| "ifcond")?;
                let tf = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_bb = self.context.append_basic_block(tf, "then");
                let else_bb = self.context.append_basic_block(tf, "else");
                let cont_bb = self.context.append_basic_block(tf, "ifcont");

                self.builder
                    .build_conditional_branch(condv, then_bb, else_bb)
                    .map_err(|_err| "build_conditional_branch")?;

                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(&truth)?;
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = match falsity.as_ref() {
                    Some(f) => self.compile_expr(f)?,
                    None => self.context.f64_type().const_float(0.),
                };
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self
                    .builder
                    .build_phi(self.context.f64_type(), "iftmp")
                    .unwrap();

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }
            Expr::For {
                assign,
                cond,
                step,
                body,
                span: _,
            } => {
                let (id, value) = if let Expr::Assign { ident, value } = assign.as_ref() {
                    (ident, value)
                } else {
                    unreachable!()
                };
                let tf = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let var_name = &id.name;
                let start_alloca = self.create_entry_block_alloca(var_name);
                let start = self.compile_expr(value)?;

                self.builder.build_store(start_alloca, start).unwrap();

                let loop_bb = self.context.append_basic_block(tf, "loop");

                self.builder.build_unconditional_branch(loop_bb).unwrap();
                self.builder.position_at_end(loop_bb);

                let old_val = self.variables.borrow_mut().remove(var_name.as_str());

                self.variables
                    .borrow_mut()
                    .insert(var_name.to_owned(), start_alloca);

                for ele in body {
                    self.compile_expr(ele)?;
                }

                let step = match step.as_ref() {
                    Some(step) => self.compile_expr(step)?,
                    None => self.context.f64_type().const_float(1.0),
                };

                let end_cond = self.compile_expr(cond)?;

                let curr_var = self
                    .builder
                    .build_load(self.context.f64_type(), start_alloca, var_name)
                    .unwrap();
                let next_var = self
                    .builder
                    .build_float_add(curr_var.into_float_value(), step, "nextvar")
                    .unwrap();

                self.builder.build_store(start_alloca, next_var).unwrap();

                let end_cond = self
                    .builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        end_cond,
                        self.context.f64_type().const_float(0.0),
                        "loopcond",
                    )
                    .unwrap();
                let after_bb = self.context.append_basic_block(tf, "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb)
                    .unwrap();
                self.builder.position_at_end(after_bb);

                self.variables.borrow_mut().remove(var_name);

                if let Some(val) = old_val {
                    self.variables.borrow_mut().insert(var_name.to_owned(), val);
                }

                Ok(self.context.f64_type().const_float(0.0))
            }
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
