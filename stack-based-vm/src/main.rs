use stack_based_vm::value::Value;
use stack_based_vm::vm::Result;
use stack_based_vm::vm::{Instruction, ADD, DIV, PUSH, VM};

fn main() -> Result<()> {
    let mut vm = VM::new();

    vm.run(vec![
        Instruction::op_code_and_operand(PUSH, Value::Int(3)),
        Instruction::op_code_and_operand(PUSH, Value::Int(1)),
        Instruction::op_code_and_operand(PUSH, Value::Int(2)),
        Instruction::op_code(ADD),
        Instruction::op_code(DIV),
    ])?;

    println!("top of stack: {:?}", vm.pop());
    Ok(())
}
