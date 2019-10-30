#include <array>
#include <iostream>
#include <vector>

class StackOverflowException : std::exception {};
class UnknownOpcodeException : std::exception {};
class TypeMismatchingException : std::exception {};

enum class ValueType {
  Int,
};

union VariantValue {
  int i;
};

class Value {
private:
  ValueType _value_type;
  VariantValue v;

public:
  static Value *Int(int i) {
    auto v = new Value;
    v->_value_type = ValueType ::Int;
    v->v.i = i;
    return v;
  }

  int get_int() {
    if (_value_type != ValueType::Int) {
      throw new TypeMismatchingException();
    }
    return this->v.i;
  }

private:
};

enum OpCode {
  OP_PUSH,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
};

struct Instruction {
  explicit Instruction(OpCode opcode) : _opcode{opcode} {}
  Instruction(OpCode opcode, Value *operand)
      : _opcode{opcode}, _operand{operand} {}

  OpCode _opcode;
  Value *_operand;
};

#define STACK_MAX 1024
#define BINARY_OP()                                                            \
  auto l = pop()->get_int();                                                   \
  auto r = pop()->get_int();

class VM {
public:
  VM(std::initializer_list<Instruction> is) : _instructions{is} {}

  void run() {
    for (auto ins : _instructions) {
      switch (ins._opcode) {
      case OP_PUSH:
        push(ins._operand);
        break;
      case OP_ADD: {
        BINARY_OP()
        push(Value::Int(l + r));
        break;
      }
      case OP_SUB: {
        BINARY_OP()
        push(Value::Int(l - r));
        break;
      }
      case OP_MUL: {
        BINARY_OP()
        push(Value::Int(l * r));
        break;
      }
      case OP_DIV: {
        BINARY_OP()
        push(Value::Int(l / r));
        break;
      }
      default:
        throw new UnknownOpcodeException();
      }
    }
  }

  Value *pop() {
    _stack_pointer--;
    return _stack[_stack_pointer];
  }
  void push(Value *v) {
    if (_stack_pointer >= _stack.size()) {
      throw new StackOverflowException();
    }
    _stack[_stack_pointer] = v;
    _stack_pointer++;
  }

private:
  std::vector<Instruction> _instructions;
  std::array<Value *, STACK_MAX> _stack{};
  int _stack_pointer = 0;
};

int main() {
  VM vm{
      Instruction(OP_PUSH, Value::Int(3)),
      Instruction(OP_PUSH, Value::Int(1)),
      Instruction(OP_PUSH, Value::Int(2)),
      Instruction(OP_ADD),
      Instruction(OP_DIV),
  };

  vm.run();
  std::cout << "VM Result: "
            << ", top of stack: " << vm.pop()->get_int() << std::endl;
  return 0;
}
