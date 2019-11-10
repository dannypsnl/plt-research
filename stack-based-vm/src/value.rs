use std::ops::{Add, Div, Mul, Sub};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Nothing,
    Int(i64),
    F32(f32),
    F64(f64),
}

impl Add for Value {
    type Output = Value;
    fn add(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Int(l), Int(r)) => Int(l + r),
            (F32(l), F32(r)) => F32(l + r),
            (F64(l) , F64(r)) => F64(l + r),
            _ => panic!("value type mismatching, since this should be checked by compiler here do not keep running.")
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Int(l), Int(r)) => Int(l - r),
            (F32(l), F32(r)) => F32(l - r),
            (F64(l) , F64(r)) => F64(l - r),
            _ => panic!("value type mismatching, since this should be checked by compiler here do not keep running.")
        }
    }
}

impl Mul for Value {
    type Output = Value;
    fn mul(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Int(l), Int(r)) => Int(l *r),
            (F32(l), F32(r)) => F32(l * r),
            (F64(l) , F64(r)) => F64(l * r),
            _ => panic!("value type mismatching, since this should be checked by compiler here do not keep running.")
        }
    }
}

impl Div for Value {
    type Output = Value;
    fn div(self, rhs: Value) -> Self::Output {
        use Value::*;
        match (self, rhs) {
            (Int(l), Int(r)) => Int(l / r),
            (F32(l), F32(r)) => F32(l / r),
            (F64(l) , F64(r)) => F64(l / r),
            _ => panic!("value type mismatching, since this should be checked by compiler here do not keep running.")
        }
    }
}
