extern crate combine;

use combine::char::{spaces, digit, letter, alpha_num, newline};
use combine::{parser, between, any, one_of, many, many1, token, try, sep_by, optional, eof, satisfy, Parser, State, Stream, ParseResult};
use combine::primitives::Consumed;
use std::collections::HashMap;

const WORD_SIZE : usize = 3;
const CHAR_BITS: u32 = 8;

#[derive(Debug, Clone, Copy)]
enum Register {
  A, B, C, D, BP, SP, PC
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

fn encode_to_text_mem(statements: Vec<Statement>, labels: &mut HashMap<String, usize>) -> Vec<Vec<Statement>> {
  println!("encode_to_text_mem: encoding {:?}", statements);
  let mut result = vec![];
  let mut basic_block = vec![];
  for statement in statements {
    match statement {
      Statement::Label(symbol) => {
        if basic_block.len() > 0 {
          println!("encode_to_text_mem: pushing basic_block {:?}", basic_block);
          result.push(basic_block);
          basic_block = vec![];
        }
        labels.insert(symbol, result.len());
      },
      Statement::Instruction(opcode, operands) => {
        basic_block.push(Statement::Instruction(opcode.clone(), operands));
        use Opcode::*;
        match opcode {
          Jeq | Jne | Jlt | Jgt | Jle | Jge | Jmp => {
            println!("encode_to_text_mem: pushing basic_block {:?}", basic_block);
            result.push(basic_block);
            basic_block = vec![];
          }, 
          _ => {},
        }
      },
    }
  }
  if basic_block.len() > 0 {
    println!("encode_to_text_mem: pushing basic_block {:?}", basic_block);
    result.push(basic_block);
  }
  result
}

fn encode_to_data_mem(statements: Vec<Statement>, label_map: &mut HashMap<String, usize>) -> Vec<u8> {
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
            let raw_bytes: [u8; 4] = unsafe {std::mem::transmute(operand)};
            result.extend_from_slice(&raw_bytes[0..WORD_SIZE]);
          } else {
            panic!("Invalid operand for .long");
          }
        },
        ".string" => {
          if let Operand::ImmS(ref operand) = operands[0] {
            result.append(&mut operand.clone().into_bytes());
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

#[derive(Default)]
struct RegisterEnv {
  a: u32,
  b: u32,
  c: u32,
  d: u32,
  bp: usize,
  sp: usize,
  pc: usize,
}

fn load_data(data: &Vec<u8>, addr: u32) -> u32 {
  let addr = (addr & 0x00ffffff) as usize;
  let mut result = 0;
  for i in 0..WORD_SIZE {
    result |= (data[addr + i] as u32) << (CHAR_BITS * i as u32);
  }
  result
}

fn store_data(data: &mut Vec<u8>, addr: u32, value: u32) {
  let addr = (addr & 0x00ffffff) as usize;
  for i in 0..WORD_SIZE {
    data[addr + i] = (value >> (i as u32)) as u8;
  }
}

fn eval(pc: u32, text: Vec<Vec<Statement>>, mut data: Vec<u8>, label_map: HashMap<String, usize>) {
  println!("Eval text! {:?}", text);
  let mut registers = vec![0u32, 0, 0, 0, 0, 0, pc];
  while (registers[Register::PC as usize] as usize) < text.len() {
    let block = &text[registers[Register::PC as usize] as usize];
    println!("Eval block! {:?}", block);
    for statement in block {
      println!("Eval statement! {:?}", statement);
      if let Statement::Instruction(ref opcode, ref operands)  = *statement {
        use Opcode::*;
        use Operand::*;
        match *opcode {
          Mov => {
            match (&operands[0], &operands[1]) {
              (&Reg(ref dst), &Reg(ref src)) => {
                registers[*dst as usize] = registers[*src as usize];
              },
              (&Reg(ref dst), &ImmI(ref src)) => {
                registers[*dst as usize] = *src as u32;
              },
              (&Reg(ref dst), &Label(ref src)) => {
                registers[*dst as usize] = label_map[src] as u32;
              },
              _ => {panic!("Illegal Mov operands")},
            }
          },
          Add => {
            match (&operands[0], &operands[1]) {
              (&Reg(ref dst), &Reg(ref src)) => {
                registers[*dst as usize] += registers[*src as usize];
              },
              (&Reg(ref dst), &ImmI(ref src)) => {
                registers[*dst as usize] += *src as u32;
              },
              (&Reg(ref dst), &Label(ref src)) => {
                registers[*dst as usize] += label_map[src] as u32;
              },
              _ => {panic!("Illegal Add operands")},
            }
          },
          Sub => {
            match (&operands[0], &operands[1]) {
              (&Reg(ref dst), &Reg(ref src)) => {
                registers[*dst as usize] -= registers[*src as usize];
              },
              (&Reg(ref dst), &ImmI(ref src)) => {
                registers[*dst as usize] -= *src as u32;
              },
              (&Reg(ref dst), &Label(ref src)) => {
                registers[*dst as usize] -= label_map[src] as u32;
              },
              _ => {panic!("Illegal Sub operands")},
            }
          },
          Load => {
            match (&operands[0], &operands[1]) {
              (&Reg(ref dst), &Reg(ref src)) => {
                registers[*dst as usize] = load_data(&data, registers[*src as usize]);
              },
              (&Reg(ref dst), &ImmI(ref src)) => {
                registers[*dst as usize] = load_data(&data, *src as u32);
              },
              (&Reg(ref dst), &Label(ref src)) => {
                registers[*dst as usize] = load_data(&data, label_map[src] as u32);
              },
              _ => {panic!("Illegal Load operands")},
            }
          },
          Store => {
            match (&operands[0], &operands[1]) {
              (&Reg(ref dst), &Reg(ref src)) => {
                 store_data(&mut data, registers[*dst as usize], registers[*src as usize]);
              },
              (&Reg(ref dst), &ImmI(ref src)) => {
                 store_data(&mut data, registers[*dst as usize], *src as u32);
              },
              (&Reg(ref dst), &Label(ref src)) => {
                 store_data(&mut data, registers[*dst as usize], label_map[src] as u32);
              },
              _ => {panic!("Illegal Load operands")},
            }
          },
          _ => {}
        }
      } else {
        panic!("Illegal statement in text. Bug of encode_to_text_mem.");
      }
    }
    registers[Register::PC as usize] += 1;
  }
  println!("regs: {:?}", registers);
}

fn main() {
  let statements = parse(TEST);
  println!("{:?}", statements);
  let (text, data) = separate_segments(statements);
  println!("separated text: {:?}", text);
  println!("separated data: {:?}", data);
  let mut label_map = HashMap::new();
  let text_mem = encode_to_text_mem(text, &mut label_map);
  let data_mem = encode_to_data_mem(data, &mut label_map);
  println!("data_mem: {:?}", &data_mem[0..10]);
  eval(0, text_mem, data_mem, label_map);
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
.text
  mov B, A
  mov C, .L0
  load D, C
  add C, 3
  load BP, C
 
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
    .Hoge:
    .long 32
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

