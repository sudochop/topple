use std::{path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, IntValue, InstructionValue, InstructionOpcode},
    OptimizationLevel, execution_engine::{ExecutionEngine, JitFunction}, memory_buffer::MemoryBuffer, IntPredicate, basic_block::BasicBlock,
};

#[allow(dead_code)]
#[derive(Debug)]
struct Located<T> {
    span: (usize, usize),
    token: T,
}

#[derive(Debug)]
enum TokenKind {
    OpPlus,
    OpMinus,
    OpMult,
    OpDiv,
    OpEq,
    OpGt,
    OpGte,
    OpLt,
    OpLte,
    OpPutchar,
    OpDup,
    OpIf,
    OpElse, 
    OpEnd,
    OpWhile,
    OpDo,
    OpLoop,

    Integer(u64),
}

type Lexed = (String, usize, usize);
type Token = Located<TokenKind>;

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
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
        let builder = context.create_builder();

        // extern push
        let _push_fn = {
            let ret_type = context.void_type();
            let fn_type = ret_type.fn_type(
                &[BasicMetadataTypeEnum::IntType(context.i64_type())],
                false,
            );
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
            let fn_type = ret_type.fn_type(&[BasicMetadataTypeEnum::IntType(context.i64_type())], false);
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

        self.builder.build_call(
            push_fn,
            &[BasicMetadataValueEnum::IntValue(val)],
            "push",
        ); 
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

        self.builder.build_call(putchar_fn, &[BasicMetadataValueEnum::IntValue(val)], "putchar");
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let source_file_path = &args.get(1).expect("file path to compile needed");
    let should_run = args[1..].contains(&"-j".to_string());

    let file = std::fs::read_to_string(source_file_path).unwrap();
    let lex = lex_with_positions(&file);
    let tokens = tokenize(lex);

    let context = Context::create();
    let tc = Compiler::new(&context, "main");   

    let mut else_blocks: Vec<(BasicBlock, InstructionValue)> = Vec::new();
    let mut then_blocks: Vec<(BasicBlock, InstructionValue)> = Vec::new();
    let mut cont_blocks: Vec<(BasicBlock, InstructionValue)> = Vec::new();

    let mut while_blocks: Vec<BasicBlock> = Vec::new();
    let mut do_blocks: Vec<(BasicBlock, InstructionValue)> = Vec::new();
    

    let ret_ty = tc.context.i64_type();
    let ret_val = ret_ty.const_int(0, false);
    let main_fn = tc.module.get_function("main").unwrap();

    let main_end_block = tc.context.append_basic_block(main_fn, "main_end");

    tc.builder.position_at_end(main_end_block);
    let jz_main_end = tc.builder.build_return(Some(&ret_val));
    let entry_block = main_fn.get_first_basic_block().unwrap();
    tc.builder.position_at_end(entry_block);
    let jz = tc.builder.build_unconditional_branch(main_end_block);
    tc.builder.position_before(&jz);

    cont_blocks.push((main_end_block, jz_main_end));

    for located in tokens {
        match located.token {
            TokenKind::Integer(i) => {
                let val = tc.context.i64_type().const_int(i, false);
                tc.push(val);
            }
            TokenKind::OpPlus => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_add(s1, s2, "add");

                tc.push(result);
            }
            TokenKind::OpMinus => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_sub(s2, s1, "sub");

                tc.push(result);
            }
            TokenKind::OpMult => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_mul(s2, s1, "sub");

                tc.push(result);
            }
            TokenKind::OpDiv => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let result = tc.builder.build_int_unsigned_div(s2, s1, "sub");

                tc.push(result);
            }
            TokenKind::OpEq => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc.builder.build_int_compare(IntPredicate::EQ, s2, s1, "ugt");
                let result = tc.builder.build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            TokenKind::OpGt => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc.builder.build_int_compare(IntPredicate::UGT, s2, s1, "ugt");
                let result = tc.builder.build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            TokenKind::OpGte => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc.builder.build_int_compare(IntPredicate::UGE, s2, s1, "ugt");
                let result = tc.builder.build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            TokenKind::OpLt => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc.builder.build_int_compare(IntPredicate::ULT, s2, s1, "ult");
                let result = tc.builder.build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            TokenKind::OpLte => {
                let s1 = tc.pop();
                let s2 = tc.pop();

                let bool = tc.builder.build_int_compare(IntPredicate::ULE, s2, s1, "ult");
                let result = tc.builder.build_int_cast_sign_flag(bool, tc.context.i64_type(), false, "cast");

                tc.push(result);
            }
            TokenKind::OpPutchar => {
                let s1 = tc.pop();
                
                tc.putchar(s1);
            }
            TokenKind::OpDup => {
                let s1 = tc.pop();
                
                tc.push(s1);
                tc.push(s1);
            }
            TokenKind::OpIf => {
                let s1 = tc.pop();

                let then_block = tc.context.append_basic_block(main_fn, "then_block");
                let else_block = tc.context.append_basic_block(main_fn, "else_block");
                let cont_block = tc.context.append_basic_block(main_fn, "cont_block");

                // JIT will complain if we have a branch right after another one.
                let curr_block = tc.builder.get_insert_block().unwrap();
                if let Some(last_inst) = curr_block.get_last_instruction() {
                    if last_inst.get_opcode() == InstructionOpcode::Br {
                        tc.builder.position_at_end(curr_block);
                        last_inst.erase_from_basic_block();
                    }
                }

                let cond = tc.builder.build_int_cast_sign_flag(s1, tc.context.bool_type(), false, "cast");
                tc.builder.build_conditional_branch(cond, then_block, else_block);

                tc.builder.position_at_end(then_block);
                let then_jz = tc.builder.build_unconditional_branch(cont_block);

                tc.builder.position_at_end(else_block);
                let else_jz = tc.builder.build_unconditional_branch(cont_block);

                let (last_cont_block, _) = *cont_blocks.last().unwrap();
                tc.builder.position_at_end(cont_block);
                let cont_jz = tc.builder.build_unconditional_branch(last_cont_block);

                tc.builder.position_before(&then_jz);

                then_blocks.push((then_block, then_jz));
                else_blocks.push((else_block, else_jz));
                cont_blocks.push((cont_block, cont_jz));
            }
            TokenKind::OpElse => {
                let (_else_block, jz) = *else_blocks.last().unwrap();
                tc.builder.position_before(&jz);
            }
            TokenKind::OpEnd => {
                then_blocks.pop().unwrap();
                else_blocks.pop().unwrap();
                let (_cont_block, cont_jz) = cont_blocks.pop().unwrap();

                tc.builder.position_before(&cont_jz);
            }
            TokenKind::OpWhile => {
                let while_block = tc.context.append_basic_block(main_fn, "while_block");
                let do_block = tc.context.append_basic_block(main_fn, "do_block");
                let cont_block = tc.context.append_basic_block(main_fn, "cont_block");

                // JIT will complain if we have a branch right after another one.
                // This should only ever be our unconditional jump for setting up cont blocks.
                let curr_block = tc.builder.get_insert_block().unwrap();
                if let Some(last_inst) = curr_block.get_last_instruction() {
                    if last_inst.get_opcode() == InstructionOpcode::Br {
                        tc.builder.position_at_end(curr_block);
                        last_inst.erase_from_basic_block();
                    }
                }

                tc.builder.build_unconditional_branch(while_block);

                // tc.builder.position_at_end(while_block);
                // let while_jz = tc.builder.build_unconditional_branch(do_block);

                tc.builder.position_at_end(do_block);
                let do_jz = tc.builder.build_unconditional_branch(while_block);

                let (last_cont_block, _) = *cont_blocks.last().unwrap();
                tc.builder.position_at_end(cont_block);
                let cont_jz = tc.builder.build_unconditional_branch(last_cont_block);

                tc.builder.position_at_end(while_block);

                while_blocks.push(while_block);
                do_blocks.push((do_block, do_jz));
                cont_blocks.push((cont_block, cont_jz));
            }
            TokenKind::OpDo => {
                let s1 = tc.pop();

                let (do_block, do_jz) = *do_blocks.last().unwrap();
                let (cont_block, _cont_jz) = *cont_blocks.last().unwrap();

                // tc.builder.position_at_end(do_block);
                let cond = tc.builder.build_int_cast_sign_flag(s1, tc.context.bool_type(), false, "cast");
                tc.builder.build_conditional_branch(cond, do_block, cont_block);

                tc.builder.position_before(&do_jz)
            }
            TokenKind::OpLoop => {
                while_blocks.pop().unwrap();
                do_blocks.pop().unwrap();
                let (_cont_block, jz_cont) = cont_blocks.pop().unwrap();

                tc.builder.position_before(&jz_cont);
            }
        }
    }

    let source_file_name = Path::file_stem(Path::new(source_file_path)).unwrap();
    let mut out_path = Path::new("out").join(source_file_name);
    out_path.set_extension("ll");

    tc.module.print_to_file(out_path).unwrap();

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

fn lex_with_positions(s: &str) -> Vec<Lexed> {
    let mut result = Vec::new();
    let mut start_of_word = None;
    let mut in_comment = false;

    for (i, c) in s.char_indices() {
        if c == '#' {
            in_comment = true;
        } else if c == '\n' {
            in_comment = false;
        }

        if in_comment {
            continue;
        }

        if c.is_whitespace() {
            if let Some(start) = start_of_word {
                // The current character is whitespace, and there's a word that just ended
                let token = s[start..i].to_string();
                let span = token.clone().len();
                result.push((token, start, span));
                start_of_word = None;
            }
        } else if start_of_word.is_none() {
            // The current character is not whitespace, and it's the start of a new word
            start_of_word = Some(i);
        }
    }

    // Handle the last word, if the string didn't end with whitespace
    if let Some(start) = start_of_word {
        let token = s[start..].to_string();
        let span = token.clone().len();
        result.push((token, start, span));
    }

    result
}

fn tokenize(lex: Vec<Lexed>) -> Vec<Token> {
    lex.iter()
        .map(|(word, start, length)| {
            let span = (*start, *length);
            match word.as_str() {
                "+" => Token {
                    span,
                    token: TokenKind::OpPlus,
                },
                "-" => Token {
                    span,
                    token: TokenKind::OpMinus,
                },
                "*" => Token {
                    span,
                    token: TokenKind::OpMult,
                },
                "/" => Token {
                    span,
                    token: TokenKind::OpDiv,
                },
                "==" => Token {
                    span,
                    token: TokenKind::OpEq,
                },
                ">" => Token {
                    span,
                    token: TokenKind::OpGt,
                },
                ">=" => Token {
                    span,
                    token: TokenKind::OpGte,
                },
                "<" => Token {
                    span,
                    token: TokenKind::OpLt,
                },
                "<=" => Token {
                    span,
                    token: TokenKind::OpLte,
                },
                "putchar" => Token {
                    span,
                    token: TokenKind::OpPutchar,
                },
                "dup" => Token {
                    span,
                    token: TokenKind::OpDup,
                },
                "if" => Token {
                    span,
                    token: TokenKind::OpIf,
                },
                "else" => Token {
                    span,
                    token: TokenKind::OpElse,
                },
                "end" => Token {
                    span,
                    token: TokenKind::OpEnd,
                },
                "while" => Token {
                    span,
                    token: TokenKind::OpWhile,
                },
                "do" => Token {
                    span,
                    token: TokenKind::OpDo,
                },
                "loop" => Token {
                    span,
                    token: TokenKind::OpLoop,
                },
                literal => {
                    let int = literal.parse::<u64>().expect("unrecognize: `{literal}` {span}");

                    Token {
                        span,
                        token: TokenKind::Integer(int),
                    }
                }
            }
        })
        .collect()
}
