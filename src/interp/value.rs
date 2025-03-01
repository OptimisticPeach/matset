use std::ops::{Add, Div, Mul, Neg, Sub};

use complex::Complex;
use mat::Matrix;
use rational::Rational;
use real::Real;
use serde::{Deserialize, Serialize};

pub mod complex;
pub mod mat;
pub mod rational;
pub mod real;

pub trait Ring:
    Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self, Output = Self>
    + Neg<Output = Self>
    + PartialEq
    + Sized
    + Clone
{
    const ONE: Self;
    const ZERO: Self;
}

pub trait Field: Ring + Div<Self, Output = Self> {
    fn inverse(self) -> Self {
        Self::ONE / self
    }
}

impl Ring for f64 {
    const ONE: Self = 1.0;

    const ZERO: Self = 0.0;
}

impl Field for f64 {
    fn inverse(self) -> Self {
        1.0 / self
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
// #[serde(tag = "func")]
pub enum Value {
    Real(Real),

    Complex(Complex<Real>),

    Mat(Matrix<Complex<Real>>),
}

impl Value {
    pub fn parse(x: &str) -> Option<Self> {
        Rational::parse(x)
            .map(|x| Value::Real(x.into()))
            .or_else(|| Complex::parse(x).map(Value::Complex))
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Real(Rational { num: 0, denom: 1 }.into())
    }
}

impl From<Rational> for Value {
    fn from(value: Rational) -> Self {
        Value::Real(value.into())
    }
}

impl From<Real> for Value {
    fn from(value: Real) -> Self {
        Value::Real(value)
    }
}

impl From<Complex<Real>> for Value {
    fn from(value: Complex<Real>) -> Self {
        Value::Complex(value)
    }
}

impl From<Matrix<Complex<Real>>> for Value {
    fn from(value: Matrix<Complex<Real>>) -> Self {
        Value::Mat(value)
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Real(x), Value::Real(y)) => (x + y).into(),
            (Value::Complex(x), Value::Real(y)) => (x + y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) + y).into(),
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
            (Value::Real(x), Value::Real(y)) => (x - y).into(),
            (Value::Complex(x), Value::Real(y)) => (x - y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) - y).into(),
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
            (Value::Real(x), Value::Real(y)) => (x * y).into(),
            (Value::Complex(x), Value::Real(y)) => (x * y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) * y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x * y).into(),

            (Value::Real(x), Value::Mat(y)) => (y.mul_lhs(Complex::from(x))).into(),
            (Value::Complex(x), Value::Mat(y)) => (y.mul_lhs(x)).into(),
            (Value::Mat(x), Value::Real(y)) => (x.mul_rhs(Complex::from(y))).into(),
            (Value::Mat(x), Value::Complex(y)) => (x.mul_rhs(y)).into(),

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
            (Value::Real(x), Value::Real(y)) => (x / y).into(),
            (Value::Complex(x), Value::Real(y)) => (x / y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) / y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x / y).into(),

            (Value::Real(x), Value::Mat(y)) => (y.mul_lhs(Complex::from(x.inverse()))).into(),
            (Value::Complex(x), Value::Mat(y)) => (y.mul_lhs(x.inverse())).into(),
            (Value::Mat(x), Value::Real(y)) => (x.mul_rhs(Complex::from(y.inverse()))).into(),
            (Value::Mat(x), Value::Complex(y)) => (x.mul_rhs(y.inverse())).into(),

            (x, y) => panic!("Division operation not supported between {x:?} and {y:?}"),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Real(x) => (-x).into(),
            Value::Mat(x) => (-x).into(),
            Value::Complex(x) => (-x).into(),
        }
    }
}
