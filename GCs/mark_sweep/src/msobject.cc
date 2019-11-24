#include "msobject.hpp"

void mark(MSObject *root) {
  if (!root->is_marked) {
    root->is_marked = true;
  }
  for (auto ref : root->refs_to) {
    mark(ref);
  }
}
