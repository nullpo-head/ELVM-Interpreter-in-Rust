extern crate combine;

use combine::char::{spaces, digit, hex_digit, letter, alpha_num, newline};
use combine::{parser, between, any, none_of, one_of, skip_many, many, many1, token, try, sep_by, optional, eof, satisfy, Parser, State, Stream, ParseResult};
use combine::primitives::{Consumed, SourcePosition};

#[derive(Debug, Clone, Copy)]
pub enum Register {
  A, B, C, D, BP, SP,
}

#[derive(Debug)]
pub enum Operand {
  Label(String),
  Reg(Register),
  ImmI(i32),
  ImmS(Vec<u32>),
}

#[derive(Debug)]
pub enum Instruction {
  Label(String),
  Instruction(Opcode, Vec<Operand>),
}

#[derive(Debug)]
pub struct Statement<'a> {
  pub instruction: Instruction,
  pub source_info: SourceInfo<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceInfo<'a> {
  pub line: i32,
  pub column: i32,
  pub filename: &'a str,
}

#[derive(Debug, Clone)]
pub enum Opcode {
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


fn inline_skipable<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a>
  where I: 'a + Stream<Item=char>
{
  Box::new(many(one_of(" \t".chars())))
}

fn skipable<'a, I>() -> Box<Parser<Input = I, Output = ()> + 'a>
  where I: 'a + Stream<Item=char>
{
  let comment = token('#').with(many::<String, _>(none_of("\n".chars())).skip(newline()));
  Box::new(spaces().and(optional(comment)).with(spaces()))
}

fn symbol<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a> 
  where I: 'a + Stream<Item=char>
{
  let symbol_parser = one_of("._".chars()).or(letter()).and(many::<String, _>(one_of("._".chars()).or(alpha_num())))
    .map(|(first, mut following)| {
      following.insert(0, first);
      following
    });
  Box::new(symbol_parser)
}

fn string_literal<'a, I>() -> Box<Parser<Input = I, Output = Vec<u32>> + 'a>
  where I: 'a + Stream<Item=char>
{
  let quoted = many::<Vec<u32>, _>(satisfy(|c| c != '"').then(|c| 
    parser(move |input| if c == '\\' {
      any().then(|d| parser(move |input| {
        match d {
        '\\' => Ok(('\\' as u32, Consumed::Consumed(input))),
        '"' => Ok(('"' as u32, Consumed::Consumed(input))),
        'n' => Ok(('\n' as u32, Consumed::Consumed(input))),
        't' => Ok(('\t' as u32, Consumed::Consumed(input))),
        'x' => many1::<String, _>(hex_digit()).map(|lit| u32::from_str_radix(&lit, 16).unwrap()).parse_stream(input),
        'b' => Ok(('\x08' as u32, Consumed::Consumed(input))),
        _ => {panic!("unknown format: {:?}", d)},
      }})).parse_stream(input)
    } else {
      Ok((c as u32, Consumed::Empty(input)))
    })
  ));
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
  let parse_op = (symbol().map(|id| match id.as_ref() {
    "A" => Operand::Reg(Register::A),
    "B" => Operand::Reg(Register::B),
    "C" => Operand::Reg(Register::C),
    "D" => Operand::Reg(Register::D),
    "SP" => Operand::Reg(Register::SP),
    "BP" => Operand::Reg(Register::BP),
    _ => Operand::Label(id)
  })
    .or(uint_literal().map(|i| Operand::ImmI(i)))
    .or(string_literal().map(|s| Operand::ImmS(s)))).skip(inline_skipable());
  sep_by::<Vec<Operand>, _, _>(parse_op, token(',').skip(inline_skipable())).parse_stream(input)
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

pub fn parse<'a>(filename: &'a str, src: &str) -> Vec<Statement<'a>> {
  let instruction = parser(opcode).skip(inline_skipable()).then(|op| parser(move |input| {
    let _: State<_> = input;
    let pos: SourcePosition = input.position;
    let src_info = SourceInfo { line: pos.line, column: pos.column, filename: filename };
    match op {
      Opcode::PseudoOp(ref pseudo_op) if *pseudo_op == ".loc" || *pseudo_op == ".file" => 
        skip_many(none_of("\n".chars()))
        .map(|_| Statement { instruction: Instruction::Instruction(op.clone(), vec![]), source_info: src_info })
        .parse_stream(input), // Skip the irregular-syntax operands of .loc and .file
      _ => 
        parser(operands)
        .map(|operands| Statement { instruction: Instruction::Instruction(op.clone(), operands), source_info: src_info })
        .parse_stream(input),
    }
  }));
  let label = parser(|input| {
    let _: State<_> = input;
    let pos: SourcePosition = input.position;
    let src_info = SourceInfo { line: pos.line, column: pos.column, filename: filename };
    symbol().and(token(':')).map(|(id, _)| Statement { instruction: Instruction::Label(id), source_info: src_info }).parse_stream(input)
  });
  let statement = try(label).or(instruction).skip(skipable());
  let mut program = many::<Vec<_>, _>(optional(skipable()).with(statement)).skip(eof());
  program.parse(State::new(src)).expect("Parse Error.").0
}
