use std::{
    fmt::Display,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
};

use complex::Complex;
use mat::Matrix;
use rational::Rational;
use serde::{Deserialize, Serialize};

pub mod complex;
pub mod mat;
pub mod rational;

pub trait Ring:
    Add<Self, Output = Self>
    + AddAssign<Self>
    + Sub<Self, Output = Self>
    + SubAssign<Self>
    + Mul<Self, Output = Self>
    + MulAssign<Self>
    + Neg<Output = Self>
    + PartialEq
    + Sized
{
    const ONE: Self;
    const ZERO: Self;
}

pub trait Field: Ring + Div<Self, Output = Self> + DivAssign<Self> {
    fn inverse(self) -> Self {
        Self::ONE / self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
// #[serde(tag = "func")]
pub enum Value {
    // #[serde(rename = "rational")]
    Rational(Rational),

    // #[serde(rename = "complex")]
    Complex(Complex),

    // #[serde(rename = "matrix")]
    Mat(Matrix<Complex>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Rational(rational) => write!(f, "{rational}"),
            Value::Complex(complex) => write!(f, "{complex}"),
            Value::Mat(matrix) => write!(f, "{matrix}"),
        }
    }
}

impl Value {
    pub fn parse(x: &str) -> Option<Self> {
        Rational::parse(x)
            .map(Into::into)
            .or_else(|| Complex::parse(x))
            .map(Into::into)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Rational(Rational { num: 0, denom: 1 })
    }
}

impl From<Rational> for Value {
    fn from(value: Rational) -> Self {
        Value::Rational(value)
    }
}

impl From<Complex> for Value {
    fn from(value: Complex) -> Self {
        Value::Complex(value)
    }
}

impl From<Matrix<Complex>> for Value {
    fn from(value: Matrix<Complex>) -> Self {
        Value::Mat(value)
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Rational(x), Value::Rational(y)) => (x + y).into(),
            (Value::Complex(x), Value::Rational(y)) => (x + y).into(),
            (Value::Rational(x), Value::Complex(y)) => (x + y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x + y).into(),
            (Value::Mat(x), Value::Mat(y)) => (x + y).into(),
            (x, y) => panic!("Addition operation not supported between {x:?} and {y:?}"),
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Rational(x), Value::Rational(y)) => (x - y).into(),
            (Value::Complex(x), Value::Rational(y)) => (x - y).into(),
            (Value::Rational(x), Value::Complex(y)) => (x - y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x - y).into(),
            (Value::Mat(x), Value::Mat(y)) => (x - y).into(),
            (x, y) => panic!("Subtraction operation not supported between {x:?} and {y:?}"),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Rational(x), Value::Rational(y)) => (x * y).into(),
            (Value::Complex(x), Value::Rational(y)) => (x * y).into(),
            (Value::Rational(x), Value::Complex(y)) => (x * y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x * y).into(),

            (Value::Rational(x), Value::Mat(y)) => (x * y).into(),
            (Value::Complex(x), Value::Mat(y)) => (x * y).into(),
            (Value::Mat(x), Value::Rational(y)) => (x * y).into(),
            (Value::Mat(x), Value::Complex(y)) => (x * y).into(),

            (Value::Mat(x), Value::Mat(y)) => {
                let mut result = x * y;

                if result.cols == 1 && result.rows == 1 {
                    result.data.pop().unwrap().into()
                } else {
                    result.into()
                }
            }
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Rational(x), Value::Rational(y)) => (x / y).into(),
            (Value::Complex(x), Value::Rational(y)) => (x / y).into(),
            (Value::Rational(x), Value::Complex(y)) => (x / y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x / y).into(),

            (Value::Mat(x), Value::Rational(y)) => (x * y.inverse()).into(),
            (Value::Mat(x), Value::Complex(y)) => (x * y.inverse()).into(),

            (x, y) => panic!("Division operation not supported between {x:?} and {y:?}"),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Rational(x) => (-x).into(),
            Value::Mat(x) => (-x).into(),
            Value::Complex(x) => (-x).into(),
        }
    }
}

impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) + rhs
    }
}

impl SubAssign for Value {
    fn sub_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) - rhs
    }
}
impl MulAssign for Value {
    fn mul_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) * rhs
    }
}
impl DivAssign for Value {
    fn div_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) / rhs
    }
}
