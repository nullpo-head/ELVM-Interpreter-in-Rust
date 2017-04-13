extern crate combine;

use combine::char::{spaces, digit, letter, alpha_num, newline};
use combine::{parser, between, any, one_of, many, many1, token, try, sep_by, optional, eof, satisfy, Parser, State, Stream, ParseResult};
use combine::primitives::Consumed;
use std::collections::HashMap;

#[derive(Debug)]
enum Register {
  A, B, C, D, BP, SP,
}

#[derive(Debug)]
enum Operand {
  Label(String),
  Reg(Register),
  ImmI(i32),
  ImmS(String),
}

#[derive(Debug)]
enum Statement {
  Label(String),
  Instruction(String, Vec<Operand>),
}

#[derive(Debug)]
enum Opcode {
  Mov,
  Add,
  Sub,
  Load,
  Store,
  Putc,
  Getc,
  Exit,
  Jeq,
  Jne,
  Jlt,
  Jgt,
  Jle,
  Jge,
  Jmp,
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
  Dump,
  PseudoOp(String),
}

fn inline_spaces<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a>
  where I: 'a + Stream<Item=char>
{
  Box::new(many(one_of(" \t".chars())))
}

fn symbol<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a> 
  where I: 'a + Stream<Item=char>
{
  let symbol_parser = token('.').or(letter()).and(many::<String, _>(token('.').or(alpha_num())))
    .map(|(first, mut following)| {
      following.insert(0, first);
      following
    });
  Box::new(symbol_parser)
}

fn string_literal<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a>
  where I: 'a + Stream<Item=char>
{
  let quoted = many::<String, _>(satisfy(|c| c != '"').then(|c| {
    parser(move |input| if c == '\\' {
      any().map(|d| match d {
        '\\' => '\\',
        '"' => '"',
        'n' => '\n',
        _ => unimplemented!(),
      }).parse_stream(input)
    } else {
      Ok((c, Consumed::Empty(input)))
    })
  }));
  Box::new(between(token('"'), token('"'), quoted))
}

fn uint_literal<'a, I>() -> Box<Parser<Input = I, Output = i32> + 'a>
  where I: 'a + Stream<Item=char>
{
  Box::new(optional(token('-')).and(many1::<String, _>(digit())).map(|(sign_lit, lit)| {
    let sign = if sign_lit.is_some() {-1} else {1};
    sign * lit.parse::<i32>().unwrap()
  }))
}

fn operands<I>(input: I) -> ParseResult<Vec<Operand>, I>
  where I: Stream<Item=char>
{
  let parse_op = symbol().map(|id| match id.as_ref() {
    "A" => Operand::Reg(Register::A),
    "B" => Operand::Reg(Register::B),
    "C" => Operand::Reg(Register::C),
    "D" => Operand::Reg(Register::D),
    "SP" => Operand::Reg(Register::SP),
    "BP" => Operand::Reg(Register::BP),
    _ => Operand::Label(id)
  })
    .or(uint_literal().map(|i| Operand::ImmI(i)))
    .or(string_literal().map(|s| Operand::ImmS(s)));
  sep_by::<Vec<Operand>, _, _>(parse_op, optional(inline_spaces()).and(token(',')).skip(inline_spaces())).parse_stream(input)
}

fn opcode<I>(input: I) -> ParseResult<Opcode, I>
  where I: Stream<Item=char>
{
  symbol().map(|symbol_str| match symbol_str.as_str() {
    "mov" => Opcode::Mov,
    "add" => Opcode::Add,
    "sub" => Opcode::Sub,
    "load" => Opcode::Load,
    "store" => Opcode::Store,
    "putc" => Opcode::Putc,
    "getc" => Opcode::Getc,
    "exit" => Opcode::Exit,
    "jeq" => Opcode::Jeq,
    "jne" => Opcode::Jne,
    "jlt" => Opcode::Jlt,
    "jgt" => Opcode::Jgt,
    "jle" => Opcode::Jle,
    "jge" => Opcode::Jge,
    "jmp" => Opcode::Jmp,
    "eq" => Opcode::Eq,
    "ne" => Opcode::Ne,
    "lt" => Opcode::Lt,
    "gt" => Opcode::Gt,
    "le" => Opcode::Le,
    "ge" => Opcode::Ge,
    "dump" => Opcode::Dump,
    _ => Opcode::PseudoOp(symbol_str.clone()),
  }).parse_stream(input)
}

fn parse(src: &str) -> Vec<Statement> {
  let instruction = optional(spaces()).with(symbol()).skip(inline_spaces()).and(parser(operands)).map(|(opcode, operands)| Statement::Instruction(opcode, operands));
  let label = optional(spaces()).with(symbol()).and(token(':')).map(|(id, _)| Statement::Label(id));
  let statement = try(label).or(instruction).skip(inline_spaces()).skip(newline()).skip(spaces());
  let mut program = many::<Vec<_>, _>(statement).skip(eof());
  program.parse(State::new(src)).unwrap().0
}

#[derive(PartialEq)]
enum Segment {
  Text,
  Data,
}

fn collect_text_labels(statements: &Vec<Statement>, labels: &mut HashMap<String, usize>) {
  let mut pc: usize = 0;
  let mut basic_block_size: u32 = 0;
  for statement in statements {
    match *statement {
      Statement::Label(ref symbol) => {
        if basic_block_size > 0 {
          pc += 1;
          basic_block_size = 0;
        }
        labels.insert((*symbol).clone(), pc);
      },
      Statement::Instruction(ref opcode, _) => {
        match (*opcode).chars().next() {
          Some('j') => {
            // This is a branch instruction. The basic block ends.
            pc += 1;
            basic_block_size = 0;
          }, 
          _ => basic_block_size += 1,
        }
      }
    }
  }
}

fn collect_data_labels(statements: &Vec<Statement>, labels: &mut HashMap<String, usize>) {
  let mut size: usize = 0;
  for statement in statements {
    match *statement {
      Statement::Label(ref symbol) => {
        labels.insert((*symbol).clone(), size);
      },
      Statement::Instruction(ref opcode, ref operands) => {
        size += match (*opcode).as_str() {
          ".long" => 3,
          ".string" => match (*operands)[0] {
            Operand::ImmS(ref str_lit) => (*str_lit).as_bytes().len(),
            _ => 0, // Actually, this is invalid but ignore now
          },
          _ => 0,
        }
      }
    }
  }
}

fn separate_segments(statements: Vec<Statement>) -> (Vec<Statement>, Vec<Statement>) {
  let mut text = vec![];
  let mut data = vec![];
  let mut seg = Segment::Text;
  for statement in statements {
    if let Statement::Instruction(ref opcode, _) = statement {
      match (*opcode).as_str() {
        ".text" => {
          seg = Segment::Text;
          continue;
        },
        ".data" => {
          seg = Segment::Data;
          continue;
        },
        _ => {},
      }
    }
    if seg == Segment::Text {
      text.push(statement);
    } else if seg == Segment::Data {
      data.push(statement);
    }
  }
  (text, data)
}

fn ecndoe_to_operations(statements: Vec<Statement>) -> Vec<Vec<Opcode>> {
  /*
  let mut operations = vec![];
  let mut basic_block = vec![];
  for statement in statements {
    if let Statement::Instruction(opstring, operands) = statement {
      match opstring.as_str() {
        "mov" => {
          basic_block.push(Operation::Mov(operands[0], operands[1]));
        },
        "jeq" => {
          basic_block.push(Operation::Jeq(operands[0], operands[1], operands[2]));
          operations.push(basic_block);
          basic_block = vec![];
        },
        _ => {},
      }
    }
  }
  */
  
  unimplemented!()
}

fn main() {
  let statements = parse(SAMPLE_PROGRAM);
  println!("{:?}", statements);
  let (text, data) = separate_segments(statements);
  let mut labels = HashMap::new();
  collect_text_labels(&text, &mut labels);
  collect_data_labels(&data, &mut labels);
  println!("{:?}", labels);
}

static SAMPLE_PROGRAM: &'static str = r#"
    .text
main:
    mov D, SP
    add D, -1
    store BP, D
    mov SP, D
    mov BP, SP
    sub SP, 1
    mov A, 0
    mov B, SP
    store A, B
    add B, 1
.data
    .L0:
    .string "Hello, world!\n"
    .Hoge:
    long 32
.text
    mov A, .L0
    mov B, BP
    add B, 16777215
    store A, B
    .L1:
    mov B, BP
    add B, 16777215
    load A, B
    mov B, A

    load A, B
    jeq .L3, A, 0
    mov B, BP
    add B, 16777215
    load A, B
    mov B, A
    load A, B
    mov D, SP
    add D, -1
    store A, D
    mov SP, D
    putc A
    add SP, 1
    .L2:
    mov B, BP
    add B, 16777215
    load A, B
    mov D, SP
    add D, -1
    store A, D
    mov SP, D
    add A, 1
    mov B, BP
    add B, 16777215
    store A, B
    load A, SP
    add SP, 1
    jmp .L1
    .L3:
    mov A, 0
    mov B, A
    exit
    exit

"#;

