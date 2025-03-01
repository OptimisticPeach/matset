use std::ops::{Add, Div, Mul, Neg, Sub};

use serde::{Deserialize, Serialize};

use super::{Field, Ring, rational::Rational};

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Real {
    Rational(Rational),
    Float(f64),
}

impl From<Rational> for Real {
    fn from(value: Rational) -> Self {
        Self::Rational(value)
    }
}

impl From<f64> for Real {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl Real {
    fn to_f64(self) -> f64 {
        match self {
            Real::Rational(rational) => rational.num as f64 / rational.denom as f64,
            Real::Float(x) => x,
        }
    }

    pub fn floatify(&mut self) {
        *self = self.to_f64().into()
    }
}

macro_rules! real_ops {
    ($op_trait:ident, $op_name:ident, $op:tt) => {
        impl $op_trait for Real {
            type Output = Real;

            fn $op_name(self, rhs: Self) -> Real {
                match (self, rhs) {
                    (Real::Rational(x), Real::Rational(y)) => Real::Rational(x $op y),
                    (x, y) => Real::Float(x.to_f64() $op y.to_f64())
                }
            }
        }
    }
}

impl Neg for Real {
    type Output = Real;

    fn neg(self) -> Self::Output {
        match self {
            Real::Rational(x) => Real::Rational(-x),
            Real::Float(x) => Real::Float(-x),
        }
    }
}

real_ops!(Add, add, +);
real_ops!(Sub, sub, -);
real_ops!(Mul, mul, *);
real_ops!(Div, div, /);

impl Ring for Real {
    const ONE: Self = Self::Rational(Rational::ONE);

    const ZERO: Self = Self::Rational(Rational::ZERO);
}

impl Field for Real {
    fn inverse(self) -> Self {
        match self {
            Real::Rational(x) => Self::Rational(x.inverse()),
            Real::Float(x) => Self::Float(x.inverse()),
        }
    }
}
