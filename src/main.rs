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
    AddressSpace, IntPredicate, OptimizationLevel,
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

        // extern printf
        let _printf_fn = {
            let ret_type = context.i8_type();
            let fn_type = ret_type.fn_type(
                &[context.i8_type().ptr_type(AddressSpace::default()).into()],
                true,
            );
            module.add_function("printf", fn_type, None)
        };

        // extern mem_ptr
        let _mem_ptr_fn = {
            let ret_type = context.i8_type().ptr_type(AddressSpace::default());
            let fn_type = ret_type.fn_type(&[], false);
            module.add_function("mem_ptr", fn_type, None)
        };

        // extern mem_set
        // let _mem_ptr_fn = {
        //     let ret_type = context.i8_type().ptr_type(AddressSpace::default());
        //     let fn_type = ret_type.fn_type(&[], false);
        //     module.add_function("mem_ptr", fn_type, None)
        // };

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

    fn mem(&self) -> IntValue<'ctx> {
        let mem_ptr_fn = self.module.get_function("mem_ptr").unwrap();
        let mem_ptr = self
            .builder
            .build_call(mem_ptr_fn, &[], "mem_ptr")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        self.builder
            .build_ptr_to_int(mem_ptr, self.context.i64_type(), "mem_ptr_as_int")
    }

    fn printint(&self, val: IntValue) {
        let printf_fn = self.module.get_function("printf").unwrap();

        let formatter = self.builder.build_global_string_ptr("%d\n", "formatter");

        self.builder.build_call(
            printf_fn,
            &[
                formatter.as_pointer_value().into(),
                BasicMetadataValueEnum::IntValue(val),
            ],
            "putchar",
        );
    }

    fn compile_exprs(&self, exprs: &Vec<Expr>) {
        let main_fn = self.module.get_function("main").unwrap();

        for located in exprs {
            match &located.node {
                ExprKind::Integer(i) => {
                    let val = self.context.i64_type().const_int(*i, false);
                    self.push(val);
                }
                ExprKind::Add => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let result = self.builder.build_int_add(s1, s2, "add");

                    self.push(result);
                }
                ExprKind::Sub => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let result = self.builder.build_int_sub(s2, s1, "sub");

                    self.push(result);
                }
                ExprKind::Mul => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let result = self.builder.build_int_mul(s2, s1, "sub");

                    self.push(result);
                }
                ExprKind::Div => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let result = self.builder.build_int_unsigned_div(s2, s1, "sub");

                    self.push(result);
                }
                ExprKind::Eq => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, s2, s1, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Gt => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::UGT, s2, s1, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Gte => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::UGE, s2, s1, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Lt => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::ULT, s2, s1, "ult");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Lte => {
                    let s1 = self.pop();
                    let s2 = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::ULE, s2, s1, "ult");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Putchar => {
                    let s1 = self.pop();

                    self.putchar(s1);
                }
                ExprKind::Dup => {
                    let s1 = self.pop();

                    self.push(s1);
                    self.push(s1);
                }
                ExprKind::Dup2 => {
                    let t = self.pop();
                    let b = self.pop();

                    self.push(b);
                    self.push(t);
                    self.push(b);
                    self.push(t);
                }
                ExprKind::Swap => {
                    let t = self.pop();
                    let b = self.pop();

                    self.push(t);
                    self.push(b);
                }
                ExprKind::Over => {
                    let t = self.pop();
                    let b = self.pop();

                    self.push(b);
                    self.push(t);
                    self.push(b);
                }
                ExprKind::Conditional {
                    then_exprs,
                    maybe_else_exprs,
                } => {
                    let s1 = self.pop();

                    let then_block = self.context.append_basic_block(main_fn, "then_block");
                    let else_block = self.context.append_basic_block(main_fn, "else_block");
                    let cont_block = self.context.append_basic_block(main_fn, "cont_block");

                    let cond = self.builder.build_int_cast_sign_flag(
                        s1,
                        self.context.bool_type(),
                        false,
                        "cast",
                    );
                    self.builder
                        .build_conditional_branch(cond, then_block, else_block);

                    self.builder.position_at_end(then_block);
                    self.compile_exprs(then_exprs);
                    if self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        self.builder.build_unconditional_branch(cont_block);
                    }

                    self.builder.position_at_end(else_block);
                    if let Some(else_exprs) = maybe_else_exprs {
                        self.compile_exprs(else_exprs);
                        if self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_terminator()
                            .is_none()
                        {
                            self.builder.build_unconditional_branch(cont_block);
                        }
                    } else {
                        self.builder.build_unconditional_branch(cont_block);
                    }

                    self.builder.position_at_end(cont_block);
                }
                ExprKind::While {
                    while_exprs,
                    do_exprs,
                } => {
                    let check_block = self.context.append_basic_block(main_fn, "check_block");
                    let body_block = self.context.append_basic_block(main_fn, "body_block");
                    let cont_block = self.context.append_basic_block(main_fn, "cont_block");

                    self.builder.build_unconditional_branch(check_block);

                    self.builder.position_at_end(check_block);
                    self.compile_exprs(while_exprs);
                    let s1 = self.pop();

                    let cond = self.builder.build_int_cast_sign_flag(
                        s1,
                        self.context.bool_type(),
                        false,
                        "cast",
                    );
                    self.builder
                        .build_conditional_branch(cond, body_block, cont_block);

                    self.builder.position_at_end(body_block);
                    self.compile_exprs(do_exprs);
                    self.builder.build_unconditional_branch(check_block);

                    self.builder.position_at_end(cont_block);
                }
                ExprKind::Mem => {
                    let mem_as_int = self.mem();
                    self.push(mem_as_int);
                }
                ExprKind::Write => {
                    let val = self.pop();
                    let addr = self.pop();

                    let ptr = self.builder.build_int_to_ptr(
                        addr,
                        self.context.i8_type().ptr_type(AddressSpace::default()),
                        "int_to_ptr",
                    );

                    let cast = self.builder.build_int_cast_sign_flag(val, self.context.i8_type(), false, "cast");

                    self.builder.build_store(ptr, cast);
                }
                ExprKind::Read => {
                    let addr = self.pop();

                    let ptr = self.builder.build_int_to_ptr(
                        addr,
                        self.context.i8_type().ptr_type(AddressSpace::default()),
                        "int_to_ptr",
                    );

                    let val = self.builder.build_load(ptr, "read_mem").into_int_value();
                    let cast = self.builder.build_int_cast_sign_flag(val, self.context.i64_type(), false, "cast");

                    self.push(cast);
                }
            }
        }
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
    let ast = parser
        .parse(&input, &mut errors, lexer)
        .map_err(|e| match e {
            lalrpop_util::ParseError::InvalidToken { .. } => todo!(),
            lalrpop_util::ParseError::UnrecognizedEof { .. } => todo!(),
            lalrpop_util::ParseError::UnrecognizedToken { .. } => todo!(),
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { error } => match error {
                lexer::LexicalError::InvalidToken { span } => {
                    dbg!(span);
                    todo!()
                }
            },
        })
        .unwrap();

    let context = Context::create();
    let tc = Compiler::new(&context, "main");

    tc.compile_exprs(&ast.exprs);

    // TODO: return final pop of stack.
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
