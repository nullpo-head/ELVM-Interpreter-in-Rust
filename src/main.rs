extern crate combine;

use combine::char::{spaces, digit, letter, alpha_num, newline};
use combine::{parser, between, any, one_of, many, many1, token, try, sep_by, optional, eof, satisfy, Parser, State, Stream, ParseResult};
use combine::primitives::Consumed;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::io;
use std::io::prelude::*;

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

fn inline_spaces<'a, I>() -> Box<Parser<Input = I, Output = String> + 'a>
  where I: 'a + Stream<Item=char>
{
  Box::new(many(one_of(" \t".chars())))
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
  let instruction = optional(spaces()).with(parser(opcode)).skip(inline_spaces()).and(parser(operands)).map(|(opcode, operands)| Statement::Instruction(opcode, operands));
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
  result.resize(usize::pow(2, (WORD_SIZE as u32) * CHAR_BITS), 0);
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
    //println!("Eval block! {:?}", block);
    'op: for statement in block {
      //println!("Eval statement! {:?}", statement);
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

fn main() {
  let statements = parse(FIZZ_BUZZ);
  let (text, data) = separate_segments(statements);
  let mut label_map = HashMap::new();
  let text_mem = encode_to_text_mem(text, &mut label_map);
  let data_mem = encode_to_data_mem(data, &mut label_map);
  eval(label_map["main"], text_mem, data_mem, label_map);
}
static TEST: &'static str = r#"
.text
main:
  add A, 1
  add A, 2
.data
  .L0:
  .long 4242
  .L1:
  .long 5656
  .L3:
  .string "H"
.text
  mov B, A
  mov C, .L0
  load D, C
  add C, 3
  load BP, .L3
  putc BP
 
"#;

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

static FIZZ_BUZZ: &'static str = r#"
	.text
my_div:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 52
	mov A, 1
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777168
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, 0
	mov B, BP
	add B, 16777167
	store A, B
	.L1:
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777192
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777166
	store A, B
	mov B, BP
	add B, 16777166
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	mov B, 1
	jne .L4, A, 0
	mov B, BP
	add B, 16777166
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	mov B, A
	ne B, 0
	.L4:
	mov A, B
	jeq .L5, A, 0
	jmp .L3
	.L5:
	mov A, BP
	add A, 16777168
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777168
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777168
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777166
	load A, B
	mov B, BP
	add B, 3
	store A, B
	.L2:
	mov B, BP
	add B, 16777167
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777167
	store A, B
	load A, SP
	add SP, 1
	jmp .L1
	.L3:
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777165
	mov A, 0
	store A, B
	.L6:
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov A, BP
	add A, 16777192
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, BP
	add B, 16777164
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777164
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ge A, B
	jeq .L9, A, 0
	mov B, BP
	add B, 16777165
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777168
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777167
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777165
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777164
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 2
	store A, B
	.L9:
	mov B, BP
	add B, 16777167
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L10, A, 0
	jmp .L8
	.L10:
	.L7:
	mov B, BP
	add B, 16777167
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	sub A, 1
	mov B, BP
	add B, 16777167
	store A, B
	load A, SP
	add SP, 1
	jmp .L6
	.L8:
	mov B, BP
	add B, 16777165
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	add A, 1
	store B, A
	load A, SP
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
__builtin_mul:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 53
	mov A, 0
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 1
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777213
	store A, B
	.L11:
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777189
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777165
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777213
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777164
	store A, B
	mov B, BP
	add B, 16777164
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	mov B, 1
	jne .L14, A, 0
	mov B, BP
	add B, 16777164
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	mov B, A
	ne B, 0
	.L14:
	mov A, B
	jeq .L15, A, 0
	jmp .L13
	.L15:
	mov B, BP
	add B, 16777164
	load A, B
	mov B, BP
	add B, 16777214
	store A, B
	.L12:
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
	jmp .L11
	.L13:
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777163
	mov A, 0
	store A, B
	.L16:
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777165
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ge A, B
	jeq .L19, A, 0
	mov B, BP
	add B, 16777163
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777189
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777163
	store A, B
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777165
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 3
	store A, B
	.L19:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L20, A, 0
	jmp .L18
	.L20:
	.L17:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	sub A, 1
	mov B, BP
	add B, 16777215
	store A, B
	load A, SP
	add SP, 1
	jmp .L16
	.L18:
	mov B, BP
	add B, 16777163
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
__builtin_div:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, BP
	add A, 16777214
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L21
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp my_div
	.L21:
	mov A, B
	add SP, 3
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
__builtin_mod:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, BP
	add A, 16777214
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L22
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp my_div
	.L22:
	mov A, B
	add SP, 3
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
print_str:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	.L23:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	jeq .L25, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	putc A
	add SP, 1
	.L24:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	jmp .L23
	.L25:
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
stringify_int:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L26:
	mov B, BP
	add B, 3
	load A, B
	sub A, 1
	mov B, BP
	add B, 3
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L28
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mod
	.L28:
	mov A, B
	add SP, 2
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 48
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L29
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_div
	.L29:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 2
	store A, B
	mov B, BP
	add B, 2
	load A, B
	jeq .L27, A, 0
	jmp .L26
	.L27:
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
print_int:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 32
	mov A, BP
	add A, 16777184
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 32
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L30
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp stringify_int
	.L30:
	mov A, B
	add SP, 2
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L31
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L31:
	mov A, B
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
stringify_hex:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	mov A, 0
	store A, B
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L32, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, 0
	sub B, A
	mov A, B
	mov B, BP
	add B, 2
	store A, B
	mov A, 1
	mov B, BP
	add B, 16777215
	store A, B
	.L32:
	.L33:
	mov B, BP
	add B, 3
	load A, B
	sub A, 1
	mov B, BP
	add B, 3
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 16
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L35
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mod
	.L35:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L36, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 48
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	jmp .L37
	.L36:
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 65
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	.L37:
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 16
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L38
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_div
	.L38:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 2
	store A, B
	mov B, BP
	add B, 2
	load A, B
	jeq .L34, A, 0
	jmp .L33
	.L34:
	mov B, BP
	add B, 16777215
	load A, B
	jeq .L39, A, 0
	mov A, 45
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	sub A, 1
	mov B, BP
	add B, 3
	store A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L39:
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isspace:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 12
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, 1
	jne .L44, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L44:
	mov A, B
	mov B, 1
	jne .L43, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 13
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L43:
	mov A, B
	mov B, 1
	jne .L42, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 9
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L42:
	mov A, B
	mov B, 1
	jne .L41, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 11
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L41:
	mov A, B
	mov B, 1
	jne .L40, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 32
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L40:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isdigit:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 48
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L45, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 57
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L45:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isxdigit:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L48
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isdigit
	.L48:
	mov A, B
	add SP, 1
	mov B, 1
	jne .L47, A, 0
	mov A, 97
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L49, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 102
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L49:
	mov A, B
	mov B, A
	ne B, 0
	.L47:
	mov A, B
	mov B, 1
	jne .L46, A, 0
	mov A, 65
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L50, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 70
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L50:
	mov A, B
	mov B, A
	ne B, 0
	.L46:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isupper:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 65
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L51, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 90
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L51:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isalpha:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 97
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L53, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 122
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L53:
	mov A, B
	mov B, 1
	jne .L52, A, 0
	mov A, 65
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, 0
	jeq .L54, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 90
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	mov B, A
	ne B, 0
	.L54:
	mov A, B
	mov B, A
	ne B, 0
	.L52:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
isalnum:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L56
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isalpha
	.L56:
	mov A, B
	add SP, 1
	mov B, 1
	jne .L55, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L57
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isdigit
	.L57:
	mov A, B
	add SP, 1
	mov B, A
	ne B, 0
	.L55:
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
abort:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 1
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	exit
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
malloc:
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
	mov B, _edata
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	mov B, _edata
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, _edata
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, _edata
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	jeq .L58, A, 0
.data
	.L59:
	.string "no memory!\n"
.text
	mov A, .L59
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L60
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L60:
	mov A, B
	add SP, 1
	mov A, 1
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	exit
	add SP, 1
	.L58:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
calloc:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L61
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L61:
	mov A, B
	add SP, 2
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L62
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp malloc
	.L62:
	mov A, B
	add SP, 1
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
free:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strtol:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 7
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	mov B, 1
	jne .L64, A, 0
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L64:
	mov A, B
	mov B, 1
	jne .L63, A, 0
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 36
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	mov B, A
	ne B, 0
	.L63:
	mov A, B
	jeq .L65, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L66, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L66:
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L65:
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L67:
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777212
	store A, B
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L69
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isspace
	.L69:
	mov A, B
	add SP, 1
	jeq .L68, A, 0
	jmp .L67
	.L68:
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 45
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L70, A, 0
	mov A, 1
	mov B, BP
	add B, 16777211
	store A, B
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777212
	store A, B
	jmp .L71
	.L70:
	mov A, 0
	mov B, BP
	add B, 16777211
	store A, B
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 43
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L72, A, 0
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777212
	store A, B
	.L72:
	.L71:
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, 1
	jne .L75, A, 0
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 16
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L75:
	mov A, B
	mov B, 0
	jeq .L74, A, 0
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 48
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L74:
	mov A, B
	mov B, 0
	jeq .L73, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 120
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, 1
	jne .L76, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 88
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L76:
	mov A, B
	mov B, A
	ne B, 0
	.L73:
	mov A, B
	jeq .L77, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, BP
	add B, 16777212
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 2
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 16
	mov B, BP
	add B, 4
	store A, B
	.L77:
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L78, A, 0
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 48
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L79, A, 0
	mov A, 8
	jmp .L80
	.L79:
	mov A, 10
	.L80:
	mov B, BP
	add B, 4
	store A, B
	.L78:
	mov B, BP
	add B, 16777211
	load A, B
	jeq .L81, A, 0
	mov B, BP
	add B, 16777209
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	jeq .L82, A, 0
	mov B, BP
	add B, 16777209
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 16777209
	store A, B
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777213
	store A, B
	.L82:
	mov B, BP
	add B, 16777209
	load A, B
	mov B, 0
	sub B, A
	mov A, B
	mov B, BP
	add B, 16777209
	store A, B
	.L81:
	mov A, 0
	mov B, BP
	add B, 16777214
	store A, B
	mov A, 0
	mov B, BP
	add B, 16777210
	store A, B
	.L83:
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L86
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isdigit
	.L86:
	mov A, B
	add SP, 1
	jeq .L87, A, 0
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 48
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 16777212
	store A, B
	jmp .L88
	.L87:
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L89
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isalpha
	.L89:
	mov A, B
	add SP, 1
	jeq .L90, A, 0
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L91
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp isupper
	.L91:
	mov A, B
	add SP, 1
	jeq .L92, A, 0
	mov A, 65
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	jmp .L93
	.L92:
	mov A, 97
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	.L93:
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 16777212
	store A, B
	jmp .L94
	.L90:
	jmp .L85
	.L94:
	.L88:
	mov B, BP
	add B, 16777212
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ge A, B
	jeq .L95, A, 0
	jmp .L85
	.L95:
	mov B, BP
	add B, 16777210
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L96, A, 0
	jmp .L84
	.L96:
	mov B, BP
	add B, 16777211
	load A, B
	jeq .L97, A, 0
	mov A, 1
	mov B, BP
	add B, 16777210
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L98
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L98:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777212
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, BP
	add B, 16777214
	store A, B
	jmp .L99
	.L97:
	mov A, 1
	mov B, BP
	add B, 16777210
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L100
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L100:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777212
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777214
	store A, B
	.L99:
	.L84:
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777212
	store A, B
	jmp .L83
	.L85:
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L101, A, 0
	mov B, BP
	add B, 16777210
	load A, B
	jeq .L102, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	jmp .L103
	.L102:
	mov B, BP
	add B, 2
	load A, B
	.L103:
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L101:
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strtoul:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L104
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strtol
	.L104:
	mov A, B
	add SP, 3
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strtoll:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L105
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strtol
	.L105:
	mov A, B
	add SP, 3
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strtoull:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L106
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strtol
	.L106:
	mov A, B
	add SP, 3
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
atoi:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
.data
	.L107:
	.string "atoi not implemented\n"
.text
	mov A, .L107
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L108
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L108:
	mov A, B
	add SP, 1
	mov A, 1
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	exit
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
getenv:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
memset:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 1
	mov A, 0
	mov B, BP
	add B, 16777215
	store A, B
	.L109:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L111, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L110:
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
	jmp .L109
	.L111:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
memcpy:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 1
	mov A, 0
	mov B, BP
	add B, 16777215
	store A, B
	.L112:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L114, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L113:
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
	jmp .L112
	.L114:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strlen:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 1
	mov A, 0
	mov B, BP
	add B, 16777215
	store A, B
	.L115:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	jeq .L117, A, 0
	.L116:
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
	jmp .L115
	.L117:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strcat:
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
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L118:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	jeq .L120, A, 0
	.L119:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	jmp .L118
	.L120:
	.L121:
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	jeq .L123, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L122:
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 3
	store A, B
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	jmp .L121
	.L123:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strcpy:
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
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L124:
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	jeq .L126, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L125:
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 3
	store A, B
	load A, SP
	add SP, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	jmp .L124
	.L126:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strcmp:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	.L127:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	mov B, 1
	jne .L130, A, 0
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	mov B, A
	ne B, 0
	.L130:
	mov A, B
	jeq .L129, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L131, A, 0
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L131:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	gt A, B
	jeq .L132, A, 0
	mov A, 1
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L132:
	.L128:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 3
	store A, B
	load A, SP
	add SP, 1
	jmp .L127
	.L129:
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strchr:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	.L133:
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	jeq .L135, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L136, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L136:
	.L134:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	jmp .L133
	.L135:
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strdup:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L137
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strlen
	.L137:
	mov A, B
	add SP, 1
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L138
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp malloc
	.L138:
	mov A, B
	add SP, 1
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L139
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strcpy
	.L139:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strtok_r:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 4
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, 0
	jeq .L140, A, 0
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, B
	mov B, BP
	add B, 2
	store A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L140:
	mov A, B
	jeq .L141, A, 0
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L141:
	.L0:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 3
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L142:
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777213
	store A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L144, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L145, A, 0
	jmp .L0
	.L145:
	.L143:
	jmp .L142
	.L144:
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L146, A, 0
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L146:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, BP
	add B, 16777212
	store A, B
	.L147:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 2
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 3
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L150:
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
	mov B, A
	load A, B
	mov B, BP
	add B, 16777213
	store A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L152, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L153, A, 0
	mov A, 0
	mov B, BP
	add B, 2
	store A, B
	jmp .L154
	.L153:
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L154:
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777212
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L152:
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L151, A, 0
	jmp .L150
	.L151:
	.L148:
	jmp .L147
	.L149:
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.data 0
charmap:
	.long 0
	.long 1
	.long 2
	.long 3
	.long 4
	.long 5
	.long 6
	.long 7
	.long 8
	.long 9
	.long 10
	.long 11
	.long 12
	.long 13
	.long 14
	.long 15
	.long 16
	.long 17
	.long 18
	.long 19
	.long 20
	.long 21
	.long 22
	.long 23
	.long 24
	.long 25
	.long 26
	.long 27
	.long 28
	.long 29
	.long 30
	.long 31
	.long 32
	.long 33
	.long 34
	.long 35
	.long 36
	.long 37
	.long 38
	.long 39
	.long 40
	.long 41
	.long 42
	.long 43
	.long 44
	.long 45
	.long 46
	.long 47
	.long 48
	.long 49
	.long 50
	.long 51
	.long 52
	.long 53
	.long 54
	.long 55
	.long 56
	.long 57
	.long 58
	.long 59
	.long 60
	.long 61
	.long 62
	.long 63
	.long 64
	.long 97
	.long 98
	.long 99
	.long 100
	.long 101
	.long 102
	.long 103
	.long 104
	.long 105
	.long 106
	.long 107
	.long 108
	.long 109
	.long 110
	.long 111
	.long 112
	.long 113
	.long 114
	.long 115
	.long 116
	.long 117
	.long 118
	.long 119
	.long 120
	.long 121
	.long 122
	.long 91
	.long 92
	.long 93
	.long 94
	.long 95
	.long 96
	.long 97
	.long 98
	.long 99
	.long 100
	.long 101
	.long 102
	.long 103
	.long 104
	.long 105
	.long 106
	.long 107
	.long 108
	.long 109
	.long 110
	.long 111
	.long 112
	.long 113
	.long 114
	.long 115
	.long 116
	.long 117
	.long 118
	.long 119
	.long 120
	.long 121
	.long 122
	.long 123
	.long 124
	.long 125
	.long 126
	.long 127
	.long 128
	.long 129
	.long 130
	.long 131
	.long 132
	.long 133
	.long 134
	.long 135
	.long 136
	.long 137
	.long 138
	.long 139
	.long 140
	.long 141
	.long 142
	.long 143
	.long 144
	.long 145
	.long 146
	.long 147
	.long 148
	.long 149
	.long 150
	.long 151
	.long 152
	.long 153
	.long 154
	.long 155
	.long 156
	.long 157
	.long 158
	.long 159
	.long 160
	.long 161
	.long 162
	.long 163
	.long 164
	.long 165
	.long 166
	.long 167
	.long 168
	.long 169
	.long 170
	.long 171
	.long 172
	.long 173
	.long 174
	.long 175
	.long 176
	.long 177
	.long 178
	.long 179
	.long 180
	.long 181
	.long 182
	.long 183
	.long 184
	.long 185
	.long 186
	.long 187
	.long 188
	.long 189
	.long 190
	.long 191
	.long 192
	.long 193
	.long 194
	.long 195
	.long 196
	.long 197
	.long 198
	.long 199
	.long 200
	.long 201
	.long 202
	.long 203
	.long 204
	.long 205
	.long 206
	.long 207
	.long 208
	.long 209
	.long 210
	.long 211
	.long 212
	.long 213
	.long 214
	.long 215
	.long 216
	.long 217
	.long 218
	.long 219
	.long 220
	.long 221
	.long 222
	.long 223
	.long 224
	.long 225
	.long 226
	.long 227
	.long 228
	.long 229
	.long 230
	.long 231
	.long 232
	.long 233
	.long 234
	.long 235
	.long 236
	.long 237
	.long 238
	.long 239
	.long 240
	.long 241
	.long 242
	.long 243
	.long 244
	.long 245
	.long 246
	.long 247
	.long 248
	.long 249
	.long 250
	.long 251
	.long 252
	.long 253
	.long 254
	.long 255
	.text
strcasecmp:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 3
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov A, charmap
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777214
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 3
	load A, B
	mov B, BP
	add B, 16777213
	store A, B
	.L155:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777213
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L156, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777214
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L157, A, 0
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L157:
	jmp .L155
	.L156:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	sub A, 1
	mov B, BP
	add B, 16777213
	store A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
strncasecmp:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 3
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L158, A, 0
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov A, charmap
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777214
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 3
	load A, B
	mov B, BP
	add B, 16777213
	store A, B
	.L159:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777213
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L161, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	sub A, 1
	mov B, BP
	add B, 16777213
	store A, B
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L161:
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777214
	store A, B
	load A, SP
	add SP, 1
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L162, A, 0
	jmp .L160
	.L162:
	mov B, BP
	add B, 4
	load A, B
	sub A, 1
	mov B, BP
	add B, 4
	store A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L160, A, 0
	jmp .L159
	.L160:
	.L158:
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
puts:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L163
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L163:
	mov A, B
	add SP, 1
	mov A, 10
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	putc A
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
vsnprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 37
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov A, 0
	mov B, BP
	add B, 16777214
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777213
	mov A, 0
	store A, B
	mov B, BP
	add B, 4
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	.L164:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	jeq .L166, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 37
	mov B, A
	load A, SP
	add SP, 1
	ne A, B
	jeq .L167, A, 0
	mov B, BP
	add B, 16777213
	load A, B
	eq A, 0
	jeq .L168, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ge A, B
	jeq .L169, A, 0
	mov A, 1
	mov B, BP
	add B, 16777213
	store A, B
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	jmp .L170
	.L169:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	.L170:
	.L168:
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777214
	store A, B
	load A, SP
	add SP, 1
	jmp .L165
	.L167:
	mov B, BP
	add B, 16777215
	load A, B
	add A, 1
	mov B, BP
	add B, 16777215
	store A, B
	mov B, A
	load A, B
	jmp .L171
	jmp .L173
	.L171:
	jne .L174, A, 100
	.L173:
	mov A, BP
	add A, 16777181
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 32
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 5
	load A, B
	add A, 1
	mov B, BP
	add B, 5
	store A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L175
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp stringify_int
	.L175:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777180
	store A, B
	jmp .L172
	jmp .L176
	.L174:
	jne .L177, A, 120
	.L176:
	mov A, BP
	add A, 16777181
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 32
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 5
	load A, B
	add A, 1
	mov B, BP
	add B, 5
	store A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L178
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp stringify_hex
	.L178:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777180
	store A, B
	jmp .L172
	jmp .L179
	.L177:
	jne .L180, A, 115
	.L179:
	mov B, BP
	add B, 5
	load A, B
	add A, 1
	mov B, BP
	add B, 5
	store A, B
	mov B, A
	load A, B
	mov B, BP
	add B, 16777180
	store A, B
	jmp .L172
	jmp .L181
	.L180:
	jne .L182, A, 99
	.L181:
	mov B, BP
	add B, 5
	load A, B
	add A, 1
	mov B, BP
	add B, 5
	store A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777181
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 0
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16777181
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, BP
	add A, 16777181
	mov B, BP
	add B, 16777180
	store A, B
	jmp .L172
	.L182:
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L184
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_int
	.L184:
	mov A, B
	add SP, 1
.data
	.L185:
	.string ": unknown format!\n"
.text
	mov A, .L185
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L186
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L186:
	mov A, B
	add SP, 1
	mov A, 1
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	exit
	add SP, 1
	.L183:
	.L172:
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777180
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L187
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strlen
	.L187:
	mov A, B
	add SP, 1
	mov B, BP
	add B, 16777179
	store A, B
	mov B, BP
	add B, 16777213
	load A, B
	eq A, 0
	jeq .L188, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777179
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	ge A, B
	jeq .L189, A, 0
	mov A, 1
	mov B, BP
	add B, 16777213
	store A, B
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	jmp .L190
	.L189:
	mov B, BP
	add B, 16777180
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L191
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp strcpy
	.L191:
	mov A, B
	add SP, 2
	.L190:
	.L188:
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777179
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, BP
	add B, 16777214
	store A, B
	.L165:
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
	jmp .L164
	.L166:
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
vsprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 256
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L192
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vsnprintf
	.L192:
	mov A, B
	add SP, 4
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
snprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, BP
	add A, 4
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L193
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vsnprintf
	.L193:
	mov A, B
	add SP, 4
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
sprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, BP
	add A, 3
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L194
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vsprintf
	.L194:
	mov A, B
	add SP, 3
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
vprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 257
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 256
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16776960
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L195
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vsnprintf
	.L195:
	mov A, B
	add SP, 4
	mov B, BP
	add B, 16776959
	store A, B
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, BP
	add A, 16776960
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16776959
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov A, BP
	add A, 16776960
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L196
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L196:
	mov A, B
	add SP, 1
	mov B, BP
	add B, 16776959
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
printf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, BP
	add A, 2
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L197
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vprintf
	.L197:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.data
	stdin:

	.long 0
	.data
	stdout:

	.long 0
	.data
	stderr:

	.long 0
	.text
fprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 1
	mov A, BP
	add A, 3
	mov B, BP
	add B, 16777215
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L198
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vprintf
	.L198:
	mov A, B
	add SP, 2
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
vfprintf:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 4
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L199
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp vprintf
	.L199:
	mov A, B
	add SP, 2
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fileno:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fopen:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, stdin
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fclose:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov A, 0
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fwrite:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 3
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 2
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 4
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L200
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L200:
	mov A, B
	add SP, 2
	mov B, BP
	add B, 16777214
	store A, B
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov A, 0
	mov B, BP
	add B, 16777213
	store A, B
	.L201:
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L203, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777213
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov B, A
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	putc A
	add SP, 1
	.L202:
	mov B, BP
	add B, 16777213
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	add A, 1
	mov B, BP
	add B, 16777213
	store A, B
	load A, SP
	add SP, 1
	jmp .L201
	.L203:
	mov B, BP
	add B, 16777214
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fputs:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L204
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp print_str
	.L204:
	mov A, B
	add SP, 1
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
fgets:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 2
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	mov B, BP
	add B, 16777215
	mov A, 0
	store A, B
	.L205:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, A
	load A, SP
	add SP, 1
	lt A, B
	jeq .L207, A, 0
	mov A, 0
	mov B, SP
	store A, B
	add B, 1
	getc A
	jne .L208, A, 0
	mov A, -1
	.L208:
	mov B, BP
	add B, 16777214
	store A, B
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 10
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, 1
	jne .L209, A, 0
	mov B, BP
	add B, 16777214
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, A
	ne B, 0
	.L209:
	mov A, B
	jeq .L210, A, 0
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L210:
	.L206:
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
	jmp .L205
	.L207:
	mov A, 0
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 2
	load A, B
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 3
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, A
	load A, SP
	add SP, 1
	sub A, B
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov C, A
	load A, SP
	mov B, A
	mov A, C
	store B, A
	load A, SP
	add SP, 1
	mov B, BP
	add B, 3
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.data 0
g_ungot:
	.long 16777215
	.data
	eof_seen:

	.long 0
	.text
fgetc:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	sub SP, 1
	mov B, g_ungot
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L211, A, 0
	mov B, eof_seen
	load A, B
	jeq .L212, A, 0
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L212:
	getc A
	jne .L213, A, 0
	mov A, -1
	.L213:
	mov B, BP
	add B, 16777215
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	mov B, eof_seen
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L211:
	mov B, g_ungot
	load A, B
	mov B, BP
	add B, 16777215
	store A, B
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, g_ungot
	store A, B
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.text
ungetc:
	mov D, SP
	add D, -1
	store BP, D
	mov SP, D
	mov BP, SP
	mov B, g_ungot
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	load A, SP
	add SP, 1
	eq A, B
	jeq .L214, A, 0
	mov B, BP
	add B, 2
	load A, B
	mov B, g_ungot
	store A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	.L214:
	mov A, 1
	mov B, 0
	sub B, A
	mov A, B
	mov B, A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
	mov SP, BP
	load A, SP
	add SP, 1
	mov BP, A
	load A, SP
	add SP, 1
	jmp A
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
	mov B, BP
	add B, 16777215
	mov A, 1
	store A, B
	.L215:
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 100
	mov B, A
	load A, SP
	add SP, 1
	le A, B
	jeq .L217, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 5
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L218
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mod
	.L218:
	mov A, B
	add SP, 2
	jeq .L219, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 3
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L220
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mod
	.L220:
	mov A, B
	add SP, 2
	jeq .L221, A, 0
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
.data
	.L222:
	.string "%d\n"
.text
	mov A, .L222
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L223
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp printf
	.L223:
	mov A, B
	add SP, 2
	jmp .L224
	.L221:
.data
	.L225:
	.string "Fizz\n"
.text
	mov A, .L225
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L226
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp printf
	.L226:
	mov A, B
	add SP, 1
	.L224:
	jmp .L227
	.L219:
.data
	.L228:
	.string "FizzBuzz\n"
.text
	mov A, .L228
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov B, BP
	add B, 16777215
	load A, B
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L229
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L229:
	mov A, B
	add SP, 2
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 3
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L230
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mod
	.L230:
	mov A, B
	add SP, 2
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, 4
	mov B, A
	load A, SP
	add SP, 1
	mov D, SP
	add D, -1
	store B, D
	mov SP, D
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L231
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp __builtin_mul
	.L231:
	mov A, B
	add SP, 2
	mov B, A
	load A, SP
	add SP, 1
	add A, B
	mov C, A
	load A, SP
	add SP, 1
	mov B, A
	mov A, C
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	mov A, .L232
	mov D, SP
	add D, -1
	store A, D
	mov SP, D
	jmp printf
	.L232:
	mov A, B
	add SP, 1
	.L227:
	.L216:
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
	jmp .L215
	.L217:
	mov A, 0
	mov B, A
	exit
	exit
"#;
