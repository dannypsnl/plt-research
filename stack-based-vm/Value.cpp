//
// Created by dannypsnl on 2019/11/8.
//

#include "Value.h"

class TypeMismatchingException : std::exception {};

int Value::get_int() {
  if (_value_type != ValueType::Int) {
    throw new TypeMismatchingException();
  }
  return this->v.i;
}

float Value::get_f32() {
  if (_value_type != ValueType::Float32) {
    throw new TypeMismatchingException();
  }
  return this->v.f32;
}
double Value::get_f64() {
  if (_value_type != ValueType::Float64) {
    throw new TypeMismatchingException();
  }
  return this->v.f64;
}
