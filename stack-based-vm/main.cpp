#include <iostream>
#include <vector>
#include <array>

enum class OpResult {
  OK,
  FAIL,
};

enum OpCode {
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
};

std::string to_string(OpResult r) {
  switch (r) {
    case OpResult::OK:
      return "OK";
    case OpResult::FAIL:
      return "FAIL";
  }
}

#define STACK_MAX 1024
#define BINARY_OP() \
  auto l = pop(); \
  auto r = pop();

class VM {
 public:
  VM(std::initializer_list<int> is): _instructions{is} {}

  OpResult run() {
    for (auto i : _instructions) {
      switch (i) {
        case OP_ADD: {
          BINARY_OP()
          push(l + r);
          break;
        }
        case OP_SUB: {
          BINARY_OP()
          push(l - r);
          break;
        }
        case OP_MUL: {
          BINARY_OP()
          push(l * r);
          break;
        }
        case OP_DIV: {
          BINARY_OP()
          push(l / r);
          break;
        }
        default:
          return OpResult::FAIL;
      }
    }
    return OpResult::OK;
  }

  int pop() {
    _stack_pointer--;
    auto r = _stack[_stack_pointer];
    return r;
  }
  OpResult push(int v) {
    if (_stack_pointer >= _stack.size()) {
      return OpResult::FAIL;
    }
    _stack[_stack_pointer] = v;
    _stack_pointer++;
    return OpResult::OK;
  }

 private:
  std::vector<int> _instructions;
  std::array<int, STACK_MAX> _stack{};
  int _stack_pointer = 0;
};

int main() {
  VM vm {OP_ADD, OP_DIV};
  vm.push(3);
  vm.push(1);
  vm.push(2);

  auto result = vm.run();
  std::cout << "VM Result: " << to_string(result) << ", top of stack: " << vm.pop() << std::endl;
  return 0;
}