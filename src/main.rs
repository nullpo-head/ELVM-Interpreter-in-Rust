extern crate getopts;
extern crate combine;

use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::io;
use std::io::prelude::*;
use getopts::Options;
use std::env;
use std::fs::File;
use std::mem;

mod parser;

use parser::*;

const WORD_SIZE : usize = 3;
const WORD_MASK : u32 = 0xffffff;
const CHAR_BITS: u32 = 8;

#[derive(PartialEq)]
enum Segment {
  Text,
  Data,
}


macro_rules! panic_with_errorinfo {
  ($info:expr, $mes:expr) => {
    panic!(format!("{}; file: {}, line: {}, column: {}", $mes, $info.filename, $info.line, $info.column))
  };
  ($info:expr, $mes:expr, $($format_params:expr),*) => {
    panic!(format!("{}; file: {}, line: {}, column: {}", 
                   format!($mes, $($format_params),*),
                   $info.filename, $info.line, $info.column))};
}

fn resolve_text_labels(text: &mut Vec<Statement>, label_map: &HashMap<String, usize>, pos_to_resolve: &Vec<usize>) {
  for pos in pos_to_resolve.iter() {
    if let Instruction::Instruction(_, ref mut operands) = text[*pos].instruction {
      for operand in operands.into_iter() {
        let resolved;
        if let Operand::Label(ref symbol) = *operand {
          resolved = Some(label_map.get(symbol).expect("Rerefence to an undeclared label"));
        } else {
          resolved = None;
        }
        if let Some(addr) = resolved {
          *operand = Operand::ImmI(*addr as i32);
        }
      }
    }
  }
}

fn resolve_data_labels(data: &mut Vec<u32>, label_map: &HashMap<String, usize>, labels_to_resolve: &Vec<(String, usize)>) {
  for &(ref symbol, ref pos) in labels_to_resolve.iter() {
    let resolved = label_map.get(symbol).expect("Rerefence to an undeclared label");
    data[*pos] = *resolved as u32;
  }
}

fn expect_dst(val: &Operand, opcode: &Opcode, src_info: &SourceInfo) {
  match *val {
    Operand::Reg(_) => {},
    _ => {panic_with_errorinfo!(src_info, "Invalid operands for {:?}", opcode);},
  };
}
fn expect_src(val: &Operand, opcode: &Opcode, src_info: &SourceInfo) {
  match *val {
    Operand::Reg(_) => {},
    Operand::ImmI(_) => {},
    Operand::Label(_) => {},
    _ => {panic_with_errorinfo!(src_info, "Invalid operands for {:?}", opcode);},
  };
}
fn expect_len(operands: &Vec<Operand>, len: usize, opcode: &Opcode, src_info: &SourceInfo) {
  if (*operands).len() != len {
    panic_with_errorinfo!(src_info, "{:?} needs {} operands", opcode, len);
  };
}

fn encode_to_text_mem<'a>(statements: Vec<Statement<'a>>, label_map: &mut HashMap<String, usize>) -> ((Vec<Statement<'a>>, Vec<usize>), Vec<usize>) {
  let mut result = vec![];
  let mut basic_block_indices = vec![0usize];
  let mut unresolved_labels = vec![];

  let mut bb_pc = 0usize;
  let mut mem_pc = 0usize;

  for statement in statements {
    assert_eq!(bb_pc, basic_block_indices.len() - 1); // basic_block_indices always has a element if we never delete elements by its defintion.
    assert_eq!(mem_pc, result.len());

    match statement.instruction {
      Instruction::Label(symbol) => {
        if mem_pc > basic_block_indices[bb_pc] {
          basic_block_indices.push(mem_pc);
          bb_pc += 1
        }
        label_map.insert(symbol, bb_pc);
      },
      Instruction::Instruction(opcode, operands) => {
        use Opcode::*;
        // Check operands' types
        match opcode {
          PseudoOp(ref pseudo_op) if *pseudo_op == ".loc" || *pseudo_op == ".file" => continue, // Skip meaningless ops
          Jeq | Jne | Jlt | Jgt | Jle | Jge => {
            expect_len(&operands, 3, &opcode, &statement.source_info);
            expect_src(&operands[0], &opcode, &statement.source_info);
            expect_dst(&operands[1], &opcode, &statement.source_info);
            expect_src(&operands[2], &opcode, &statement.source_info);
          }, 
          Jmp => {
            expect_len(&operands, 1, &opcode, &statement.source_info);
            expect_src(&operands[0], &opcode, &statement.source_info);
          },
          Getc => {
            expect_len(&operands, 1, &opcode, &statement.source_info);
            expect_dst(&operands[0], &opcode, &statement.source_info);
          },
          Putc => {
            expect_len(&operands, 1, &opcode, &statement.source_info);
            expect_src(&operands[0], &opcode, &statement.source_info);
          },
          Exit | Dump => {
            expect_len(&operands, 0, &opcode, &statement.source_info);
          },
          _ => {
            expect_len(&operands, 2, &opcode, &statement.source_info);
            expect_dst(&operands[0], &opcode, &statement.source_info);
            expect_src(&operands[1], &opcode, &statement.source_info);
          },
        }
        for operand in operands.iter() {
          if let Operand::Label(_) = *operand {
            unresolved_labels.push(mem_pc);
          }
        }
        // Separate a basic block if the op is jump
        match opcode {
          Jeq | Jne | Jlt | Jgt | Jle | Jge | Jmp => {
            basic_block_indices.push(mem_pc + 1);
            bb_pc += 1;
          },
          _ => {}
        }
        result.push(Statement { instruction: Instruction::Instruction(opcode, operands), source_info: statement.source_info });
        mem_pc += 1;
      },
    }
  }
  if mem_pc > basic_block_indices[bb_pc] {
    basic_block_indices.push(mem_pc);
    bb_pc += 1
  }
  label_map.insert(String::from("_etext"), bb_pc);
  ((result, basic_block_indices), unresolved_labels)
}

fn encode_to_data_mem(statements: Vec<Statement>, label_map: &mut HashMap<String, usize>) -> (Vec<u32>, Vec<(String, usize)>) {
  let mut result = vec![];
  let mut unresolved_labels = vec![];

  for statement in statements {
    match statement.instruction {
      Instruction::Label(symbol) => {
        label_map.insert(symbol, result.len());
        continue;
      },
      Instruction::Instruction(opcode, mut operands) => {
        if let Opcode::PseudoOp(ref pseudo_op) = opcode {
          match pseudo_op.as_str() {
            ".long" => {
              expect_len(&operands, 1, &opcode, &statement.source_info);
              match operands.remove(0) {
                Operand::ImmI(operand) => {
                  result.push(operand as u32);
                },
                Operand::Label(label) => {
                  let loc = label_map.get(&label);
                  match loc {
                    None => {
                      unresolved_labels.push((label, result.len()));
                      result.push(0); // Dummy
                    },
                    Some(loc) => result.push(*loc as u32),
                  }
                },
                _ => panic_with_errorinfo!(statement.source_info, "Invalid operand for .long"),
              }
            },
            ".string" => {
              expect_len(&operands, 1, &opcode, &statement.source_info);
              if let Operand::ImmS(mut operand) = operands.remove(0) {
                result.append(&mut operand);
                result.push(0);
              } else {
                panic_with_errorinfo!(statement.source_info, "Invalid operand for .string");
              }
            },
            opstr => {panic_with_errorinfo!(statement.source_info, "Invalid pseudo op in .data segment: {:?}", opstr);},
          }
        } else {
          panic_with_errorinfo!(statement.source_info, "Invalid opcode in .data segment: {:?}", opcode);
        }
      }
    }
  }
  let edata_addr = result.len();
  let edata_val = result.len() as u32 + 1;
  label_map.insert(String::from("_edata"), edata_addr);
  result.push(edata_val);

  result.resize(2 << (WORD_SIZE as u32 * CHAR_BITS), 0);
  (result, unresolved_labels)
}

fn extract_subsection(statement: &Instruction, src_info: &SourceInfo) -> i32 {
  match *statement {
    Instruction::Instruction(ref opcode, ref operands) => {
      if operands.len() == 0 {
        return 0;
      } else {
        expect_len(operands, 1, &opcode, &src_info);
        return match operands[0] {
          Operand::ImmI(subsec) => subsec,
          _ => panic_with_errorinfo!(src_info, ".text or .data expects only integer as its operands for subsection")
        }
      }
    },
    _ => {
      panic_with_errorinfo!(src_info, "Internal Error. 'extract_subsection' expects pseudo_op");
    }
  }
}

fn separate_segments(statements: Vec<Statement>) -> (Vec<Statement>, Vec<Statement>) {
  let mut text = vec![vec![]];
  let mut data = vec![vec![]];
  let mut seg = Segment::Text;
  let mut subsection = 0;
  for statement in statements {
    if let Instruction::Instruction(Opcode::PseudoOp(ref pseudo_opcode), _) = statement.instruction {
      match (*pseudo_opcode).as_str() {
        ".text" => {
          seg = Segment::Text;
          subsection = extract_subsection(&statement.instruction, &statement.source_info) as usize;
          continue;
        },
        ".data" => {
          seg = Segment::Data;
          subsection = extract_subsection(&statement.instruction, &statement.source_info) as usize;
          continue;
        },
        _ => {},
      }
    }
    let seg_to_write = if seg == Segment::Text {&mut text} else {&mut data};
    if subsection >= seg_to_write.len() {
      for _ in seg_to_write.len()..(subsection + 1) {
        seg_to_write.push(vec![]);
      }
    }
    seg_to_write[subsection].push(statement);
  }
  let flatten = |xs:Vec<Vec<_>>| xs.into_iter().flat_map (move |x| x).collect::<Vec<_>>();
  (flatten(text), flatten(data))
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
struct EvalEnv<'a> {
  mem_pc: usize,
  registers: RegisterEnv,
  text: Vec<Statement<'a>>,
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

fn src(src: &Operand, registers: &RegisterEnv) -> u32 {
  match *src {
    Operand::Reg(ref name) => registers[*name],
    Operand::ImmI(ref val) => *val as u32,
    _ => panic!("not src"),
  }
}

fn dst<'a>(dst: &Operand, registers: &'a mut RegisterEnv) -> &'a mut u32 {
  match *dst {
    Operand::Reg(ref name) => &mut registers[*name],
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

fn dump_regs(env: &EvalEnv) {
  println!("PC={} A={} B={} C={} D={} BP={} SP={}", env.text[env.mem_pc].source_info.line, env.registers[Register::A], env.registers[Register::B], env.registers[Register::C], env.registers[Register::D], env.registers[Register::BP], env.registers[Register::SP]);
}

fn eval(pc: usize, text: Vec<Statement>, basic_block_table: Vec<usize>, data: Vec<u32>, label_map: HashMap<String, usize>, verbose: bool) {
  let mut env = EvalEnv {mem_pc: pc, text: text, data: data, label_map: label_map, ..Default::default()};
  while env.mem_pc < env.text.len() {
    if verbose {
      dump_regs(&env);
    }

    if let Instruction::Instruction(ref opcode, ref operands)  = env.text[env.mem_pc].instruction {
      use Opcode::*;
      match *opcode {
        Mov => *dst(&operands[0], &mut env.registers) = src(&operands[1], &env.registers),
        Add => *dst(&operands[0], &mut env.registers) = dst(&operands[0], &mut env.registers).wrapping_add(src(&operands[1], &env.registers)) & WORD_MASK,
        Sub => *dst(&operands[0], &mut env.registers) = dst(&operands[0], &mut env.registers).wrapping_sub(src(&operands[1], &env.registers)) & WORD_MASK,
        Load => *dst(&operands[0], &mut env.registers) = env.data[(src(&operands[1], &env.registers) & WORD_MASK) as usize],
        Store => {
          let addr = (src(&operands[1], &env.registers) & WORD_MASK) as usize;
          env.data[addr] = src(&operands[0], &env.registers);
        },
        Putc => {
          io::stdout().write(&[src(&operands[0], &env.registers) as u8]).expect("write error");
        },
        Getc => {
          let mut buf = [0; 1];
          io::stdin().read(&mut buf).expect("read error");
          *dst(&operands[0], &mut env.registers) = buf[0] as u32;
        },
        Eq | Ne | Lt | Gt | Le | Ge => {
          let d = *dst(&operands[0], &mut env.registers) & WORD_MASK;
          let s = src(&operands[1], &env.registers) & WORD_MASK;
          *dst(&operands[0], &mut env.registers) = if compare(opcode, d, s) {1} else {0};
        },
        Jeq | Jne | Jlt | Jgt | Jle | Jge => {
          let j = src(&operands[0], &env.registers) & WORD_MASK;
          let d = *dst(&operands[1], &mut env.registers) & WORD_MASK;
          let s = src(&operands[2], &env.registers) & WORD_MASK;
          if compare(opcode, d, s) {
            env.mem_pc = basic_block_table[j as usize];
            continue;
          }
        },
        Jmp => {
          let target = basic_block_table.get((src(&operands[0], &env.registers) & WORD_MASK) as usize);
          env.mem_pc = match target {
            Some(t) => *t,
            None => {
              dump_regs(&env);
              panic!("Illegal jump target; target: {}", src(&operands[0], &env.registers) & WORD_MASK);
            }
          };
          continue;
        },
        Exit => std::process::exit(0),
        Dump => {/* Do Nothing */},
        PseudoOp(_) => unreachable!(),
      }
    } else {
      panic!("Illegal statement in text. Bug of encode_to_text_mem.");
    }
    env.mem_pc += 1;
  }
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let mut opts = Options::new();
  opts.optflag("h", "help", "");
  opts.optflag("v", "verbose", "");
  let mut optparse = opts.parse(&args[1..]).expect("Option parsing failed");
  if optparse.free.len() < 1 || optparse.opt_present("h") {
    println!("Usage: {} EIR_FILE", args[0]);
    return;
  }

  let filename = mem::replace(&mut optparse.free[0], "".to_string());
  let mut eir_str = String::new();
  {
    let mut file = File::open(&filename).expect("Could not open file");
    file.read_to_string(&mut eir_str).expect("Could not read file");
  }
  
  let statements = parse(&filename, &eir_str);
  let (text, data) = separate_segments(statements);
  let mut label_map = HashMap::new();
  let ((mut text_mem, basic_block_table), unresolved_text_labels) = encode_to_text_mem(text, &mut label_map);
  let (mut data_mem, unresolved_data_labels) = encode_to_data_mem(data, &mut label_map);
  resolve_text_labels(&mut text_mem, &label_map, &unresolved_text_labels);
  resolve_data_labels(&mut data_mem, &label_map, &unresolved_data_labels);

  let start = match label_map.get("main") {
    Some(main) => *main,
    None => 0,
  };
  eval(basic_block_table[start], text_mem, basic_block_table, data_mem, label_map, optparse.opt_present("v"));
}
