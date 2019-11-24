#pragma once

#include <string>
#include <vector>

class MSObject {
  MSObject();

public:
  MSObject(std::string name) : name{name}, is_marked{false}, refs_to{} {}

public:
  bool is_marked;
  std::string name;
  std::vector<MSObject *> refs_to;
};

void mark(MSObject *);
