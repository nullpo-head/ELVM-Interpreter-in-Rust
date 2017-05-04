extern crate combine;
extern crate getopts;

use combine::char::{spaces, digit, hex_digit, letter, alpha_num, newline};
use combine::{parser, between, any, none_of, one_of, skip_many, many, many1, token, try, sep_by, optional, eof, satisfy, Parser, State, Stream, ParseResult};
use combine::primitives::Consumed;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::io;
use std::io::prelude::*;
use getopts::Options;
use std::env;
use std::fs::File;

const WORD_SIZE : usize = 3;
const WORD_MASK : u32 = 0xffffff;
const CHAR_BITS: u32 = 8;

#[derive(Debug, Clone, Copy)]
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
  Instruction(Opcode, Vec<Operand>),
}

#[derive(Debug, Clone)]
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
  sep_by::<Vec<Operand>, _, _>(parse_op, optional(inline_skipable()).and(token(',')).skip(inline_skipable())).parse_stream(input)
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
  let instruction = parser(opcode).skip(inline_skipable()).then(|op| parser(move |input| {
    match op {
      Opcode::PseudoOp(ref pseudo_op) if *pseudo_op == ".loc" || *pseudo_op == ".file" => skip_many(none_of("\n".chars())).map(|_| Statement::Instruction(op.clone(), vec![])).parse_stream(input), // Skip the irregular-syntax operands of .loc and .file
      _ => parser(operands).map(|operands| Statement::Instruction(op.clone(), operands)).parse_stream(input),
    }
  }));
  let label = symbol().and(token(':')).map(|(id, _)| Statement::Label(id));
  let statement = try(label).or(instruction).skip(inline_skipable()).skip(newline());
  let mut program = many::<Vec<_>, _>(optional(skipable()).with(statement).skip(skipable())).skip(eof());
  program.parse(State::new(src)).unwrap().0
}

#[derive(PartialEq)]
enum Segment {
  Text,
  Data,
}

fn encode_to_text_mem(statements: Vec<Statement>, label_map: &mut HashMap<String, usize>) -> Vec<Vec<Statement>> {
  let mut result = vec![];
  let mut basic_block = vec![];
  for (line, statement) in statements.into_iter().enumerate() {
    match statement {
      Statement::Label(symbol) => {
        if basic_block.len() > 0 {
          result.push(basic_block);
          basic_block = vec![];
        }
        label_map.insert(symbol, result.len());
      },
      Statement::Instruction(opcode, operands) => {
        use Opcode::*;
        {
          let ensure_dst = |val| {
            match val {
              &Operand::Reg(_) => {},
              _ => {panic!("Invalid operands for {:?}: {}", opcode, line);},
            }
          };
          let ensure_src = |val| {
            match val {
              &Operand::Reg(_) => {},
              &Operand::ImmI(_) => {},
              &Operand::Label(_) => {},
              _ => {panic!("Invalid operands for {:?}: {}", opcode, line);},
            }
          };
          let ensure_len = |operands: &Vec<_>, len| {
            if (*operands).len() != len {
              panic!("{:?} needs {} operands: {}", opcode, len, line);
            }
          };
          match opcode {
            Jeq | Jne | Jlt | Jgt | Jle | Jge => {
              ensure_len(&operands, 3);
              ensure_src(&operands[0]);
              ensure_dst(&operands[1]);
              ensure_src(&operands[2]);
            }, 
            Jmp => {
              ensure_len(&operands, 1);
              ensure_src(&operands[0]);
            },
            Getc => {
              ensure_len(&operands, 1);
              ensure_dst(&operands[0]);
            },
            Putc => {
              ensure_len(&operands, 1);
              ensure_src(&operands[0]);
            },
            Exit => {
              ensure_len(&operands, 0);
            },
            PseudoOp(ref pseudo_op) if *pseudo_op == ".loc" || *pseudo_op == ".file" => {/* Do Nothing */},
            _ => {
              ensure_len(&operands, 2);
              ensure_dst(&operands[0]);
              ensure_src(&operands[1]);
            },
          }
        }
        basic_block.push(Statement::Instruction(opcode.clone(), operands));
        match opcode {
          Jeq | Jne | Jlt | Jgt | Jle | Jge | Jmp => {
            result.push(basic_block);
            basic_block = vec![];
          },
          _ => {}
        }
      },
    }
  }
  if basic_block.len() > 0 {
    result.push(basic_block);
  }
  result
}

fn encode_to_data_mem(statements: Vec<Statement>, label_map: &mut HashMap<String, usize>) -> Vec<u32> {
  let mut result = vec![];
  for statement in statements {
    if let Statement::Label(symbol) = statement {
      label_map.insert(symbol, result.len());
      continue;
    }
    if let Statement::Instruction(Opcode::PseudoOp(opcode), operands) = statement {
      match opcode.as_str() {
        ".long" => {
          if let Operand::ImmI(operand) = operands[0] {
            result.push(operand as u32);
          } else {
            panic!("Invalid operand for .long");
          }
        },
        ".string" => {
          if let Operand::ImmS(ref operand) = operands[0] {
            result.append(&mut operand.chars().map(|c| c as u32).collect());
          } else {
            panic!("Invalid operand for .string");
          }
        },
        opstr => {panic!("Invalid opcode in .data segment: {}", opstr)},
      };
    }
  }
  result.resize(2 << (WORD_SIZE as u32 * CHAR_BITS), 0);
  result
}

fn separate_segments(statements: Vec<Statement>) -> (Vec<Statement>, Vec<Statement>) {
  let mut text = vec![];
  let mut data = vec![];
  let mut seg = Segment::Text;
  for statement in statements {
    if let Statement::Instruction(Opcode::PseudoOp(ref opcode), _) = statement {
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

#[derive(Default, Debug)]
struct RegisterEnv {
  a: u32,
  b: u32,
  c: u32,
  d: u32,
  bp: u32,
  sp: u32,
}

#[derive(Default, Debug)]
struct EvalEnv {
  pc: usize,
  registers: RegisterEnv,
  data: Vec<u32>,
  label_map: HashMap<String, usize>,
}

impl Index<Register> for RegisterEnv {
  type Output = u32;

  fn index(&self, register: Register) -> &u32 {
    match register {
      Register::A => &self.a,
      Register::B => &self.b,
      Register::C => &self.c,
      Register::D => &self.d,
      Register::BP => &self.bp,
      Register::SP => &self.sp,
    }
  }
}

impl IndexMut<Register> for RegisterEnv {
  fn index_mut<'a>(&'a mut self, register: Register) -> &'a mut u32 {
    match register {
      Register::A => &mut self.a,
      Register::B => &mut self.b,
      Register::C => &mut self.c,
      Register::D => &mut self.d,
      Register::BP => &mut self.bp,
      Register::SP => &mut self.sp,
    }
  }
}

fn src(src: &Operand, env: &EvalEnv) -> u32 {
  match *src {
    Operand::Reg(ref name) => env.registers[*name],
    Operand::ImmI(ref val) => *val as u32,
    Operand::Label(ref name) => env.label_map[name] as u32,
    _ => panic!("not src"),
  }
}

fn dst<'a>(dst: &Operand, env: &'a mut EvalEnv) -> &'a mut u32 {
  match *dst {
    Operand::Reg(ref name) => &mut env.registers[*name],
    _ => panic!("not dst"),
  }
}

fn compare(cmp: &Opcode, dst: u32, src: u32) -> bool {
  match *cmp {
    Opcode::Eq | Opcode::Jeq => dst == src,
    Opcode::Ne | Opcode::Jne => dst != src,
    Opcode::Lt | Opcode::Jlt => dst < src,
    Opcode::Gt | Opcode::Jgt => dst > src,
    Opcode::Le | Opcode::Jle => dst <= src,
    Opcode::Ge | Opcode::Jge => dst >= src,
    _ => panic!("unreachable"),
  }
}

fn eval(pc: usize, text: Vec<Vec<Statement>>, data: Vec<u32>, label_map: HashMap<String, usize>) {
  let mut env = EvalEnv {pc: pc, data: data, label_map: label_map, ..Default::default()};
  'block: while env.pc < text.len() {
    let block = &text[env.pc];
    'op: for statement in block {
      if let Statement::Instruction(ref opcode, ref operands)  = *statement {
        use Opcode::*;
        match *opcode {
          Mov => *dst(&operands[0], &mut env) = src(&operands[1], &env),
          Add => *dst(&operands[0], &mut env) = dst(&operands[0], &mut env).wrapping_add(src(&operands[1], &env)),
          Sub => *dst(&operands[0], &mut env) = dst(&operands[0], &mut env).wrapping_sub(src(&operands[1], &env)),
          Load => *dst(&operands[0], &mut env) = env.data[(src(&operands[1], &env) & WORD_MASK) as usize],
          Store => {
            let addr = (src(&operands[1], &env) & WORD_MASK) as usize;
            env.data[addr] = src(&operands[0], &env);
          },
          Putc => {io::stdout().write(&[src(&operands[0], &env) as u8]).unwrap();},
          Getc => {
            let mut buf = [0; 1];
            let c = io::stdin().read(&mut buf).expect("Input error");
            *dst(&operands[0], &mut env) = c as u32;
          },
          Eq | Ne | Lt | Gt | Le | Ge => {
            let d= *dst(&operands[0], &mut env) & WORD_MASK;
            let s= src(&operands[1], &env) & WORD_MASK;
            *dst(&operands[0], &mut env) = if compare(opcode, d, s) {1} else {0};
          },
          Jeq | Jne | Jlt | Jgt | Jle | Jge => {
            let j = src(&operands[0], &env) & WORD_MASK;
            let d = *dst(&operands[1], &mut env) & WORD_MASK;
            let s = src(&operands[2], &env) & WORD_MASK;
            if compare(opcode, d, s) {
              env.pc = j as usize;
              continue 'block;
            }
          },
          Jmp => {
            env.pc = (src(&operands[0], &env) & WORD_MASK) as usize;
            continue 'block;
          },
          Exit => break 'block,
          Dump => println!("{:?}", env),
          PseudoOp(_) => unreachable!(),
        }
      } else {
        panic!("Illegal statement in text. Bug of encode_to_text_mem.");
      }
    }
    env.pc += 1;
  }
}

fn interpret(eir: &str) {
  let statements = parse(eir);
  let (text, data) = separate_segments(statements);
  let mut label_map = HashMap::new();
  let text_mem = encode_to_text_mem(text, &mut label_map);
  let data_mem = encode_to_data_mem(data, &mut label_map);
  eval(label_map["main"], text_mem, data_mem, label_map);
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let mut opts = Options::new();
  opts.optflag("h", "help", "");
  if args.len() < 2 || opts.parse(&args[1..]).unwrap().opt_present("h") {
    println!("Usage: {} EIR_FILE", args[0]);
    return;
  }

  let mut eir_str = String::new();
  {
    let mut file = File::open(&args[1]).unwrap();
    file.read_to_string(&mut eir_str).unwrap();
  }
  
  interpret(&eir_str);
}
