use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use serde::{Deserialize, Serialize};

use super::{Field, Ring, rational::Rational};

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
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
    pub fn to_f64(self) -> f64 {
        match self {
            Real::Rational(rational) => rational.num as f64 / rational.denom as f64,
            Real::Float(x) => x,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Real::Rational(rational) => rational.is_zero(),
            Real::Float(x) => *x == 0.0,
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
real_ops!(Rem, rem, %);

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

impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Real::Rational(x), Real::Rational(y)) => x == y,
            (x, y) => x.to_f64() == y.to_f64(),
        }
    }
}

impl PartialOrd for Real {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Real::Rational(x), Real::Rational(y)) => x.partial_cmp(y),
            (x, y) => x.to_f64().partial_cmp(&y.to_f64()),
        }
    }
}

macro_rules! unary_fn {
    ($($name:ident),+) => {
        impl Real {
            $(
                pub fn $name(self) -> Real {
                    Real::Float(self.to_f64().$name())
                }
            )+
        }
    }
}

unary_fn!(
    sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, asinh, acosh, atanh, exp, log2, log10, ln
);

impl Real {
    pub fn pow(self, other: Self) -> Self {
        match (self, other) {
            (Real::Rational(x), Real::Rational(y)) => x
                .pow(y)
                .map(Real::Rational)
                .unwrap_or_else(|| self.to_f64().powf(other.to_f64()).into()),
            (x, y) => x.to_f64().powf(y.to_f64()).into(),
        }
    }

    pub fn nthroot(self, other: Self) -> Self {
        self.pow(other.inverse())
    }

    pub fn floor(self) -> Self {
        match self {
            Real::Rational(rational) => Real::Rational(rational.floor()),
            Real::Float(x) => Real::Float(x.floor()),
        }
    }

    pub fn fract(self) -> Self {
        match self {
            Real::Rational(rational) => rational.fract().into(),
            Real::Float(x) => x.fract().into(),
        }
    }
}
