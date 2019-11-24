#include <iostream>
#include <string>
#include <vector>

#include "msobject.hpp"

class VM {
  std::vector<MSObject *> heap;
  int _MAX_HEAP = 10; // 10 objects
public:
  VM() : heap{} {}
  MSObject *new_object();
  MSObject *new_object_with(std::string name);

  void dumpHeap() {
    for (auto o : heap) {
      std::cout << o->name << '\n';
    }
    std::cout << '\n';
  }

  template <typename T> void free(T it);
  void collect();
  void sweep();
};

template <typename T> void VM::free(T it) {
  delete *it;
  this->heap.erase(it);
}

MSObject *VM::new_object() { return this->new_object_with("new_object"); }
MSObject *VM::new_object_with(std::string name) {
  if (heap.size() >= _MAX_HEAP) {
    return nullptr;
  }
  auto newObj = new MSObject(name);
  this->heap.push_back(newObj);
  return newObj;
}

void VM::sweep() {
  for (auto it = this->heap.begin(); it != this->heap.end();) {
    if (!(*it)->is_marked) {
      delete *it;
      this->heap.erase(it);
    } else {
      (*it)->is_marked = false;
      ++it;
    }
  }
}

int main() {
  VM *vm = new VM();
  auto root = vm->new_object_with("ROOT");
  auto obj1 = vm->new_object_with("OBJ1");
  auto obj2 = vm->new_object_with("OBJ2");
  root->refs_to.push_back(obj1);
  root->refs_to.push_back(obj2);

  vm->dumpHeap();

  mark(root);
  vm->sweep();
  vm->dumpHeap();

  root->refs_to.pop_back();
  mark(root);
  vm->sweep();
  vm->dumpHeap();
}
