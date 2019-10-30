#include <array>
#include <iostream>
#include <vector>

class StackOverflowException : std::exception {};
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
};

class VM;

struct Instruction {
  explicit Instruction(void (VM::*opcode)()) : _opcode{opcode} {}
  Instruction(void (VM::*opcode)(), Value *operand)
      : _opcode{opcode}, _operand{operand} {}

  void (VM::*_opcode)();
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
      _opcode = ins._opcode;
      _operand = ins._operand;
      (*this.*_opcode)();
    }
  }

  void execute_push() { push(_operand); }
  void execute_add() {
    BINARY_OP()
    push(Value::Int(l + r));
  }
  void execute_sub() {
    BINARY_OP()
    push(Value::Int(l - r));
  }
  void execute_mul() {
    BINARY_OP()
    push(Value::Int(l * r));
  }
  void execute_div() {
    BINARY_OP()
    push(Value::Int(l / r));
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
  void (VM::*_opcode)();
  Value *_operand = nullptr;
  std::vector<Instruction> _instructions;
  std::array<Value *, STACK_MAX> _stack{};
  int _stack_pointer = 0;
};

void (VM::*OP_PUSH)() = &VM::execute_push;
void (VM::*OP_ADD)() = &VM::execute_add;
void (VM::*OP_SUB)() = &VM::execute_sub;
void (VM::*OP_MUL)() = &VM::execute_mul;
void (VM::*OP_DIV)() = &VM::execute_div;

int main() {
  VM vm{
      Instruction(OP_PUSH, Value::Int(3)),
      Instruction(OP_PUSH, Value::Int(1)),
      Instruction(OP_PUSH, Value::Int(2)),
      Instruction(OP_ADD),
      Instruction(OP_DIV),
  };

  vm.run();
  std::cout << "top of stack: " << vm.pop()->get_int() << std::endl;
  return 0;
}
