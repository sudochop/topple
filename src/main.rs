mod ast;
mod lexer;
mod tokens;

use std::{collections::HashMap, path::Path};

use ast::{Block, BlockKind, Declaration, DeclarationKind, Expr, Macro};
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
    macros: HashMap<String, Macro>,
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

        // extern dbg
        let _dbg_fn = {
            let ret_type = context.void_type();
            let fn_type = ret_type.fn_type(&[], false);
            module.add_function("dbg", fn_type, None)
        };

        Compiler {
            context,
            module,
            builder,
            execution_engine,
            macros: HashMap::new(),
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

    fn putint(&self, val: IntValue) {
        let printf_fn = self.module.get_function("printf").unwrap();

        let putint_str_fmt = self
            .builder
            .build_global_string_ptr("%d", "formatter")
            .as_pointer_value();

        self.builder.build_call(
            printf_fn,
            &[putint_str_fmt.into(), BasicMetadataValueEnum::IntValue(val)],
            "putint",
        );
    }

    fn dbg(&self) {
        let dbg_fn = self.module.get_function("dbg").unwrap();

        self.builder.build_call(dbg_fn, &[], "dbg");
    }

    fn gather_macros(&mut self, ast: Vec<Declaration>) -> Vec<Declaration> {
        ast.iter()
            .filter_map(|located| match &located.node {
                DeclarationKind::Macro(mac) => {
                    self.macros.insert(mac.name.clone(), mac.to_owned());
                    None
                }
                _ => Some(located.to_owned()),
            })
            .collect()
    }

    fn expand_macros(&self, ast: Vec<Declaration>) -> (Vec<Declaration>, bool) {
        let mut expanded = false;

        let decls = ast
            .iter()
            .map(|decl| match &decl.node {
                DeclarationKind::Macro(Macro { name, .. }) => {
                    assert!(false, "macro `{name}` not removed from ast");
                    decl.to_owned()
                }
                DeclarationKind::Function { name, body } => Declaration {
                    location: decl.location,
                    node: DeclarationKind::Function {
                        name: name.to_string(),
                        body: Block {
                            location: body.location,
                            node: BlockKind {
                                exprs: body
                                    .node
                                    .exprs
                                    .iter()
                                    .map(|expr| {
                                        let (expr, was_expanded) =
                                            self.expand_macro(expr.to_owned());
                                        expanded = was_expanded;
                                        expr
                                    })
                                    .collect(),
                            },
                        },
                    },
                },
            })
            .collect::<Vec<Declaration>>();

        (decls, expanded)
    }

    fn expand_macro(&self, expr: Expr) -> (Expr, bool) {
        let mut expanded = false;

        let expr = match expr.node {
            ExprKind::MacroCall { name, args } => {
                let mac = self.macros.get(&name).unwrap();
                let mut bindings: HashMap<String, Expr> = HashMap::new();

                mac.args.iter().zip(args.iter()).for_each(|(n, e)| {
                    bindings.insert(n.to_string(), e.to_owned());
                });

                let ast: Vec<Expr> = mac
                    .body
                    .node
                    .exprs
                    .iter()
                    .map(|expr| self.expand_bindings(expr.to_owned(), &bindings))
                    .collect();

                let expansion: Vec<Expr> = ast.iter().map(|expr| {
                    self.expand_macro(expr.to_owned())
                }).map(|(e, _b)| e).collect();

                expanded = true;

                Expr {
                    location: expr.location,
                    node: ExprKind::Block(Block {
                        location: expr.location,
                        node: BlockKind { exprs: expansion }
                    })
                }
            }
            ExprKind::Conditional {
                then_block,
                maybe_else_block,
            } => Expr {
                location: expr.location,
                node: ExprKind::Conditional {
                    then_block: Block {
                        location: then_block.location,
                        node: BlockKind {
                            exprs: then_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| {
                                    let (expr, was_expanded) = self.expand_macro(expr.to_owned());
                                    expanded = was_expanded;
                                    expr
                                })
                                .collect(),
                        },
                    },
                    maybe_else_block: maybe_else_block.map(|else_block| Block {
                        location: else_block.location,
                        node: BlockKind {
                            exprs: else_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| {
                                    let (expr, was_expanded) = self.expand_macro(expr.to_owned());
                                    expanded = was_expanded;
                                    expr
                                })
                                .collect(),
                        },
                    }),
                },
            },
            ExprKind::While {
                while_exprs,
                do_block,
            } => Expr {
                location: expr.location,
                node: ExprKind::While {
                    while_exprs: while_exprs
                        .iter()
                        .map(|expr| {
                            let (expr, was_expanded) = self.expand_macro(expr.to_owned());
                            expanded = was_expanded;
                            expr
                        })
                        .collect(),
                    do_block: Block {
                        location: do_block.location,
                        node: BlockKind {
                            exprs: do_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| {
                                    let (expr, was_expanded) = self.expand_macro(expr.to_owned());
                                    expanded = was_expanded;
                                    expr
                                })
                                .collect(),
                        },
                    },
                },
            },
            ExprKind::Block(block) => Expr {
                location: expr.location,
                node: ExprKind::Block(Block {
                    location: block.location,
                    node: BlockKind {
                        exprs: block.node
                            .exprs
                            .iter()
                            .map(|expr| {
                                let (expr, was_expanded) = self.expand_macro(expr.to_owned());
                                expanded = was_expanded;
                                expr
                            })
                            .collect(),
                    }
                }),
            },
            _ => expr,
        };

        (expr, expanded)
    }

    fn expand_bindings(&self, expr: Expr, bindings: &HashMap<String, Expr>) -> Expr {
        match expr.node {
            ExprKind::Binding(name) => bindings.get(&name).unwrap().to_owned(),
            ExprKind::While {
                while_exprs,
                do_block,
            } => Expr {
                location: expr.location,
                node: ExprKind::While {
                    while_exprs: while_exprs
                        .iter()
                        .map(|expr| self.expand_bindings(expr.to_owned(), bindings))
                        .collect(),
                    do_block: Block {
                        location: do_block.location,
                        node: BlockKind {
                            exprs: do_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| self.expand_bindings(expr.to_owned(), bindings))
                                .collect(),
                        },
                    },
                },
            },
            ExprKind::Conditional {
                then_block,
                maybe_else_block,
            } => Expr {
                location: expr.location,
                node: ExprKind::Conditional {
                    then_block: Block {
                        location: then_block.location,
                        node: BlockKind {
                            exprs: then_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| self.expand_bindings(expr.to_owned(), bindings))
                                .collect(),
                        },
                    },
                    maybe_else_block: maybe_else_block.map(|else_block| Block {
                        location: else_block.location,
                        node: BlockKind {
                            exprs: else_block
                                .node
                                .exprs
                                .iter()
                                .map(|expr| self.expand_bindings(expr.to_owned(), bindings))
                                .collect(),
                        },
                    }),
                },
            },
            ExprKind::Block(block) => Expr {
                location: expr.location,
                node: ExprKind::Block(Block {
                    location: block.location,
                    node: BlockKind {
                        exprs: block.node
                            .exprs
                            .iter()
                            .map(|expr| self.expand_bindings(expr.to_owned(), bindings))
                            .collect(),
                    }
                }),
            },
            _ => expr,
        }
    }

    fn compile(&self, ast: Vec<Declaration>) {
        for located in ast.iter() {
            match &located.node {
                DeclarationKind::Function { name, .. } => {
                    let ret_type = self.context.void_type();
                    let fn_type = ret_type.fn_type(&[], false);
                    self.module.add_function(name, fn_type, None);
                }
                DeclarationKind::Macro(Macro { name, .. }) => {
                    assert!(false, "macro `{name}` not removed from ast");
                }
            }
        }
        for located in ast.iter() {
            match &located.node {
                DeclarationKind::Function { name, body } => {
                    let fn_value = self.module.get_function(name).unwrap();
                    let entry = self.context.append_basic_block(fn_value, "entry");
                    self.builder.position_at_end(entry);

                    self.compile_exprs(&body.node.exprs);

                    self.builder.build_return(None);
                }
                DeclarationKind::Macro(Macro { name, .. }) => {
                    assert!(false, "macro `{name}` not removed from ast");
                }
            }
        }
    }

    fn compile_exprs(&self, exprs: &Vec<Expr>) {
        let curr_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        for located in exprs {
            match &located.node {
                ExprKind::FnCall(name) => {
                    let func = self.module.get_function(name).unwrap();
                    self.builder.build_call(func, &[], name);
                }
                ExprKind::MacroCall { name, .. } => {
                    assert!(false, "macro `{name}` not expanded");
                }
                ExprKind::Block(block) => {
                    self.compile_exprs(&block.node.exprs);
                }
                ExprKind::Binding(name) => {
                    unimplemented!("binding: `{name}`");
                }
                ExprKind::Integer(i) => {
                    let val = self.context.i64_type().const_int(*i, false);
                    self.push(val);
                }
                ExprKind::Add => {
                    let top = self.pop();
                    let mid = self.pop();

                    let result = self.builder.build_int_add(top, mid, "add");

                    self.push(result);
                }
                ExprKind::Sub => {
                    let top = self.pop();
                    let mid = self.pop();

                    let result = self.builder.build_int_sub(mid, top, "sub");

                    self.push(result);
                }
                ExprKind::Mul => {
                    let top = self.pop();
                    let mid = self.pop();

                    let result = self.builder.build_int_mul(mid, top, "sub");

                    self.push(result);
                }
                ExprKind::Div => {
                    let top = self.pop();
                    let mid = self.pop();

                    let result = self.builder.build_int_unsigned_div(mid, top, "sub");

                    self.push(result);
                }
                ExprKind::Eq => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, mid, top, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Gt => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::UGT, mid, top, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Gte => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::UGE, mid, top, "ugt");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Lt => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::ULT, mid, top, "ult");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Lte => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self
                        .builder
                        .build_int_compare(IntPredicate::ULE, mid, top, "ult");
                    let result = self.builder.build_int_cast_sign_flag(
                        bool,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

                    self.push(result);
                }
                ExprKind::Or => {
                    let top = self.pop();
                    let mid = self.pop();

                    let bool = self.builder.build_or(mid, top, "or");

                    self.push(bool)
                }
                ExprKind::Putchar => {
                    let top = self.pop();

                    self.putchar(top);
                }
                ExprKind::Putint => {
                    let top = self.pop();

                    self.putint(top);
                }
                ExprKind::Dbg => {
                    self.dbg();
                }
                ExprKind::Dup => {
                    let top = self.pop();

                    self.push(top);
                    self.push(top);
                }
                ExprKind::Swap => {
                    let top = self.pop();
                    let mid = self.pop();

                    self.push(top);
                    self.push(mid);
                }
                ExprKind::Over => {
                    let top = self.pop();
                    let mid = self.pop();

                    self.push(mid);
                    self.push(top);
                    self.push(mid);
                }
                ExprKind::Rot => {
                    let top = self.pop();
                    let mid = self.pop();
                    let bot = self.pop();

                    self.push(mid);
                    self.push(top);
                    self.push(bot);
                }
                ExprKind::Drop => {
                    self.pop();
                }
                ExprKind::Conditional {
                    then_block,
                    maybe_else_block,
                } => {
                    let top = self.pop();

                    let then_basic_block =
                        self.context.append_basic_block(curr_fn, "then_basic_block");
                    let else_basic_block =
                        self.context.append_basic_block(curr_fn, "else_basic_block");
                    let cont_basic_block =
                        self.context.append_basic_block(curr_fn, "cont_basic_block");

                    let cond = self.builder.build_int_cast_sign_flag(
                        top,
                        self.context.bool_type(),
                        false,
                        "cast",
                    );
                    self.builder
                        .build_conditional_branch(cond, then_basic_block, else_basic_block);

                    self.builder.position_at_end(then_basic_block);
                    self.compile_exprs(&then_block.node.exprs);
                    if self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        self.builder.build_unconditional_branch(cont_basic_block);
                    }

                    self.builder.position_at_end(else_basic_block);
                    if let Some(else_exprs) = maybe_else_block {
                        self.compile_exprs(&else_exprs.node.exprs);
                        if self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_terminator()
                            .is_none()
                        {
                            self.builder.build_unconditional_branch(cont_basic_block);
                        }
                    } else {
                        self.builder.build_unconditional_branch(cont_basic_block);
                    }

                    self.builder.position_at_end(cont_basic_block);
                }
                ExprKind::While {
                    while_exprs,
                    do_block,
                } => {
                    let check_basic_block = self
                        .context
                        .append_basic_block(curr_fn, "check_basic_block");
                    let body_basic_block =
                        self.context.append_basic_block(curr_fn, "body_basic_block");
                    let cont_basic_block =
                        self.context.append_basic_block(curr_fn, "cont_basic_block");

                    self.builder.build_unconditional_branch(check_basic_block);

                    self.builder.position_at_end(check_basic_block);
                    self.compile_exprs(while_exprs);
                    let top = self.pop();

                    let cond = self.builder.build_int_cast_sign_flag(
                        top,
                        self.context.bool_type(),
                        false,
                        "cast",
                    );
                    self.builder
                        .build_conditional_branch(cond, body_basic_block, cont_basic_block);

                    self.builder.position_at_end(body_basic_block);
                    self.compile_exprs(&do_block.node.exprs);
                    self.builder.build_unconditional_branch(check_basic_block);

                    self.builder.position_at_end(cont_basic_block);
                    self.pop();
                }
                ExprKind::Mem => {
                    let mem_as_int = self.mem();
                    self.push(mem_as_int);
                }
                ExprKind::Store => {
                    let val = self.pop();
                    let addr = self.pop();

                    let ptr = self.builder.build_int_to_ptr(
                        addr,
                        self.context.i8_type().ptr_type(AddressSpace::default()),
                        "int_to_ptr",
                    );

                    let cast = self.builder.build_int_cast_sign_flag(
                        val,
                        self.context.i8_type(),
                        false,
                        "cast",
                    );

                    self.builder.build_store(ptr, cast);
                }
                ExprKind::Load => {
                    let addr = self.pop();

                    let ptr = self.builder.build_int_to_ptr(
                        addr,
                        self.context.i8_type().ptr_type(AddressSpace::default()),
                        "int_to_ptr",
                    );

                    let val = self.builder.build_load(ptr, "read_mem").into_int_value();
                    let cast = self.builder.build_int_cast_sign_flag(
                        val,
                        self.context.i64_type(),
                        false,
                        "cast",
                    );

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
            lalrpop_util::ParseError::UnrecognizedToken { token: (start, token, end), expected } => {
                dbg!(token);
                dbg!(&input[start - 10..end + 10]);
                dbg!(&input[start..end]);
                dbg!(expected);
                unimplemented!("unrecognized token");
            }
            lalrpop_util::ParseError::ExtraToken { .. } => todo!(),
            lalrpop_util::ParseError::User { error } => match error {
                lexer::LexicalError::InvalidToken { span } => {
                    dbg!(&input[span.start - 10..span.end + 10]);
                    dbg!(&input[span.start..span.end]);
                    todo!()
                }
            },
        })
        .unwrap();

    let context = Context::create();
    let mut tc = Compiler::new(&context, "main");

    let ast = tc.gather_macros(ast.declarations);

    let mut expanded = true;
    let mut expanded_ast = ast.to_owned();

    while expanded {
        let (ast, was_expanded) = tc.expand_macros(expanded_ast);
        expanded_ast = ast;
        expanded = was_expanded;
    }

    tc.compile(expanded_ast);

    // TODO: return final pop of stack.
    // let ret_ty = tc.context.i64_type();
    // let ret_val = ret_ty.const_int(0, false);
    // tc.builder.build_return(Some(&ret_val));

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
