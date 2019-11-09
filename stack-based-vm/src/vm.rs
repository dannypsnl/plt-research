use crate::value::Value;

const STACK_MAX: usize = 1024;

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub struct VM {
    operand: Value,
    stack: [Value; STACK_MAX],
    stack_pointer: usize,
}

impl VM {
    pub fn new() -> VM {
        VM {
            operand: Value::Nothing,
            stack: [Value::Nothing; STACK_MAX],
            stack_pointer: 0,
        }
    }
    pub fn run(&mut self, instructions: Vec<Instruction>) -> Result<()> {
        for ins in instructions {
            self.operand = ins.operand;
            (ins.op_code)(self)?;
        }
        Ok(())
    }
    pub fn pop(&mut self) -> Value {
        self.stack_pointer -= 1;
        self.stack[self.stack_pointer]
    }
    pub fn push(&mut self, v: Value) -> Result<()> {
        if self.stack_pointer >= self.stack.len() {
            Err(RuntimeError::StackOverflow)
        } else {
            self.stack[self.stack_pointer] = v;
            self.stack_pointer += 1;
            Ok(())
        }
    }

    fn execute_push(&mut self) -> Result<()> {
        self.push(self.operand)
    }
    fn execute_add(&mut self) -> Result<()> {
        let (l, r) = (self.pop(), self.pop());
        self.push(l + r)
    }
    fn execute_sub(&mut self) -> Result<()> {
        let (l, r) = (self.pop(), self.pop());
        self.push(l - r)
    }
    fn execute_mul(&mut self) -> Result<()> {
        let (l, r) = (self.pop(), self.pop());
        self.push(l * r)
    }
    fn execute_div(&mut self) -> Result<()> {
        let (l, r) = (self.pop(), self.pop());
        self.push(l / r)
    }
}

type OpCode = fn(&mut VM) -> Result<()>;

pub const PUSH: OpCode = VM::execute_push;
pub const ADD: OpCode = VM::execute_add;
pub const SUB: OpCode = VM::execute_sub;
pub const MUL: OpCode = VM::execute_mul;
pub const DIV: OpCode = VM::execute_div;

pub struct Instruction {
    operand: Value,
    op_code: OpCode,
}

impl Instruction {
    pub fn op_code(op_code: OpCode) -> Instruction {
        Instruction {
            operand: Value::Nothing,
            op_code,
        }
    }
    pub fn op_code_and_operand(op_code: OpCode, operand: Value) -> Instruction {
        Instruction { operand, op_code }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    StackOverflow,
}
