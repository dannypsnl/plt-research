//
// Created by dannypsnl on 2019/11/8.
//

#ifndef STACK_BASED_VM__VALUE_H_
#define STACK_BASED_VM__VALUE_H_

#include <exception>

class TypeMismatchingException;

enum class ValueType {
  Int,
  Float32,
  Float64,
};

union VariantValue {
  int i;
  float f32;
  double f64;
};

class Value {
private:
  ValueType _value_type;
  VariantValue v;

public:
  static Value *Int(int i) {
    auto v = new Value;
    v->_value_type = ValueType::Int;
    v->v.i = i;
    return v;
  }
  static Value *Float32(float f) {
    auto v = new Value;
    v->_value_type = ValueType::Float32;
    v->v.f32 = f;
    return v;
  }
  static Value *Float64(double d) {
    auto v = new Value;
    v->_value_type = ValueType::Float64;
    v->v.f64 = d;
    return v;
  }

  int get_int();
  float get_f32();
  double get_f64();
};

#endif // STACK_BASED_VM__VALUE_H_
