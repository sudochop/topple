mod ast;
mod lexer;
mod tokens;

use std::path::Path;

use ast::Expr;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    memory_buffer::MemoryBuffer,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, IntValue},
    IntPredicate, OptimizationLevel,
};

use lexer::Lexer;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub topple);
use topple::SourceUnitParser;

use crate::ast::ExprKind;

type Main = unsafe extern "C" fn() -> u64;

struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let builder = context.create_builder();

        // extern push
        let _push_fn = {
            let ret_type = context.void_type();
            let fn_type =
                ret_type.fn_type(&[BasicMetadataTypeEnum::IntType(context.i64_type())], false);
            module.add_function("push", fn_type, None)
        };

        // extern peek
        let _peek_fn = {
            let ret_type = context.i64_type();
            let fn_type = ret_type.fn_type(&[], false);
            module.add_function("peek", fn_type, None)
        };

        // extern peek
        let _pop_fn = {
            let ret_type = context.i64_type();
            let fn_type = ret_type.fn_type(&[], false);
            module.add_function("pop", fn_type, None)
        };

        // extern putchar
        let _putchar_fn = {
            let ret_type = context.i64_type();
            let fn_type =
                ret_type.fn_type(&[BasicMetadataTypeEnum::IntType(context.i64_type())], false);
            module.add_function("putchar", fn_type, None)
        };

        let main_ret_type = context.i64_type();
        let main_fn_type = main_ret_type.fn_type(&[], false);
        let main_fn_value = module.add_function("main", main_fn_type, None);
        let main_entry = context.append_basic_block(main_fn_value, "entry");
        builder.position_at_end(main_entry);

        Compiler {
            context,
            module,
            builder,
            execution_engine,
        }
    }

    fn push(&self, val: IntValue) {
        let push_fn = self.module.get_function("push").unwrap();

        self.builder
            .build_call(push_fn, &[BasicMetadataValueEnum::IntValue(val)], "push");
    }

    fn pop(&self) -> IntValue<'ctx> {
        let pop_fn = self.module.get_function("pop").unwrap();

        self.builder
            .build_call(pop_fn, &[], "pop")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }

    fn putchar(&self, val: IntValue) {
        let putchar_fn = self.module.get_function("putchar").unwrap();

        self.builder.build_call(
            putchar_fn,
            &[BasicMetadataValueEnum::IntValue(val)],
            "putchar",
        );
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let source_file_path = &args.get(1).expect("file path to compile needed");
    let should_run = args[1..].contains(&"-j".to_string());

    let input = std::fs::read_to_string(source_file_path).unwrap();
    let lexer = Lexer::new(&input[..]);
    let parser = SourceUnitParser::new();
    let mut errors = Vec::new();
    let ast = parser.parse(&input, &mut errors, lexer).unwrap();

    let context = Context::create();
    let tc = Compiler::new(&context, "main");

    compile_exprs(&ast.exprs, &tc);

    let ret_ty = tc.context.i64_type();
    let ret_val = ret_ty.const_int(0, false);
    tc.builder.build_return(Some(&ret_val));

    let source_file_name = Path::file_stem(Path::new(source_file_path)).unwrap();
    let mut out_path = Path::new("out").join(source_file_name);
    out_path.set_extension("ll");

    tc.module.print_to_file(out_path).unwrap();
    tc.module.verify().unwrap();

    if should_run {
        let mem = MemoryBuffer::create_from_file(Path::new("src/stack.ll")).unwrap();
        let module = tc.context.create_module_from_ir(mem).unwrap();
        tc.execution_engine.add_module(&module).unwrap();

        let _output = unsafe {
            let main: JitFunction<Main> = tc.execution_engine.get_function("main").unwrap();
            main.call()
        };
    }
}

fn compile_exprs(exprs: &Vec<Expr>, tc: &Compiler) {
    let main_fn = tc.module.get_function("main").unwrap();

    for located in exprs {
        match &located.node {
            ExprKind::Integer(i) => {
                let val = tc.context.i64_type().const_int(*i, false);
                tc.push(val);
            }
            ExprKind::Add => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_add(s1, s2, "add");

                tc.push(result);
            }
            ExprKind::Sub => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_sub(s2, s1, "sub");

                tc.push(result);
            }
            ExprKind::Mul => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_mul(s2, s1, "sub");

                tc.push(result);
            }
            ExprKind::Div => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_unsigned_div(s2, s1, "sub");

                tc.push(result);
            }
            ExprKind::Eq => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc
                    .builder
                    .build_int_compare(IntPredicate::EQ, s2, s1, "ugt");
                let result =
                    tc.builder
                        .build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            ExprKind::Gt => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc
                    .builder
                    .build_int_compare(IntPredicate::UGT, s2, s1, "ugt");
                let result =
                    tc.builder
                        .build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            ExprKind::Gte => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc
                    .builder
                    .build_int_compare(IntPredicate::UGE, s2, s1, "ugt");
                let result =
                    tc.builder
                        .build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            ExprKind::Lt => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc
                    .builder
                    .build_int_compare(IntPredicate::ULT, s2, s1, "ult");
                let result =
                    tc.builder
                        .build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            ExprKind::Lte => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc
                    .builder
                    .build_int_compare(IntPredicate::ULE, s2, s1, "ult");
                let result =
                    tc.builder
                        .build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            ExprKind::Putchar => {
                let s1 = tc.pop();

                tc.putchar(s1);
            }
            ExprKind::Dup => {
                let s1 = tc.pop();

                tc.push(s1);
                tc.push(s1);
            }
            ExprKind::Conditional {
                then_exprs,
                maybe_else_exprs,
            } => {
                let s1 = tc.pop();

                let then_block = tc.context.append_basic_block(main_fn, "then_block");
                let else_block = tc.context.append_basic_block(main_fn, "else_block");
                let cont_block = tc.context.append_basic_block(main_fn, "cont_block");

                let cond =
                    tc.builder
                        .build_int_cast_sign_flag(s1, tc.context.bool_type(), false, "cast");
                tc.builder
                    .build_conditional_branch(cond, then_block, else_block);

                tc.builder.position_at_end(then_block);
                compile_exprs(then_exprs, tc);
                if tc
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    tc.builder.build_unconditional_branch(cont_block);
                }

                tc.builder.position_at_end(else_block);
                if let Some(else_exprs) = maybe_else_exprs {
                    compile_exprs(else_exprs, tc);
                    if tc
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        tc.builder.build_unconditional_branch(cont_block);
                    }
                } else {
                    tc.builder.build_unconditional_branch(cont_block);
                }

                tc.builder.position_at_end(cont_block);
            }
            ExprKind::While {
                while_exprs,
                do_exprs,
            } => {
                let check_block = tc.context.append_basic_block(main_fn, "check_block");
                let body_block = tc.context.append_basic_block(main_fn, "body_block");
                let cont_block = tc.context.append_basic_block(main_fn, "cont_block");

                tc.builder.build_unconditional_branch(check_block);

                tc.builder.position_at_end(check_block);
                compile_exprs(while_exprs, tc);
                let s1 = tc.pop();

                let cond =
                    tc.builder
                        .build_int_cast_sign_flag(s1, tc.context.bool_type(), false, "cast");
                tc.builder
                    .build_conditional_branch(cond, body_block, cont_block);

                tc.builder.position_at_end(body_block);
                compile_exprs(do_exprs, tc);
                tc.builder.build_unconditional_branch(check_block);

                tc.builder.position_at_end(cont_block);
            }
        }
    }
}
