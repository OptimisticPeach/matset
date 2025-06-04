use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use anyhow::{Result, bail};
use complex::Complex;
use function::Function;
use mat::Matrix;
use rational::Rational;
use real::Real;
use serde::{Deserialize, Serialize};

pub mod complex;
pub mod function;
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
pub enum Value {
    One,
    NegOne,
    Zero,

    Real(Real),

    Complex(Complex<Real>),

    Mat(Matrix<Value>),

    Function(Function),
}

impl Value {
    pub fn parse(mut x: &str) -> Option<Self> {
        if x.len() == 0 {
            return None;
        }

        let mut neg = false;

        if x.chars().next().unwrap() == '−' {
            neg = true;

            x = &x[3..];
        }
        let result = Rational::parse(x)
            .map(|x| Value::Real(x.into()))
            .or_else(|| x.parse::<f64>().ok().map(Real::Float).map(Value::Real))
            .or_else(|| Complex::parse(x).map(Value::Complex));

        if neg { result.map(|x| -x) } else { result }
    }

    pub fn realify(self) -> Self {
        match self {
            Value::Complex(complex) => {
                match complex.imag {
                    Real::Rational(r) => {
                        if r == Rational::ZERO {
                            return Value::Real(complex.real);
                        }
                    }
                    Real::Float(f) => {
                        if f == 0.0 {
                            return Value::Real(complex.real);
                        }
                    }
                }

                self
            }
            x => x,
        }
    }

    pub fn floatify(&mut self) {
        match self {
            Value::NegOne => *self = (-1).into(),
            Value::One => *self = 1.into(),
            Value::Zero => *self = 0.into(),
            Value::Real(x) => x.floatify(),
            Value::Complex(x) => x.floatify(),
            Value::Mat(x) => x.floatify(),
            Value::Function(_) => unimplemented!(),
        }
    }

    pub fn pow(self, other: Self) -> Result<Self> {
        let power = match other {
            Value::NegOne => return Ok(self.inverse()),
            Value::One => return Ok(self),
            Value::Zero => return Ok(Value::One),
            Value::Real(real) => Complex::from(real),
            Value::Complex(complex) => complex,
            Value::Mat(_) => bail!("Cannot have matrix in exponent!"),
            Value::Function(_) => bail!("Cannot have function in exponent!"),
        };

        match self {
            Value::One => Ok(Value::One),
            Value::Zero => Ok(Value::Zero),
            Value::NegOne => Ok(Complex::from(Real::from(Rational::from(-1)))
                .pow(power)
                .into()),
            Value::Real(real) => Ok(Complex::from(real).pow(power).into()),
            Value::Complex(complex) => Ok(complex.pow(power).into()),
            Value::Mat(matrix) => {
                let Complex {
                    real: Real::Rational(Rational { num, denom: 1 }),
                    imag: Real::Rational(Rational { num: 0, denom: 1 }),
                } = power
                else {
                    bail!("Matrix powers must be positive integers!")
                };

                if num < 0 {
                    bail!("Matrix powers must be positive integers!")
                }

                let mut num = num.abs() as u128;

                if num == 0 {
                    return Ok(Real::ONE.into());
                }

                let mut acc = None;
                let mut shifted = matrix;

                while num != 0 {
                    if num & 1 == 1 {
                        acc = Some(
                            acc.map(|x| x * shifted.clone())
                                .unwrap_or_else(|| shifted.clone()),
                        );
                    }

                    shifted = shifted.clone() * shifted;
                    num >>= 1;
                }

                Ok(acc.unwrap().into())
            }
            Value::Function(f) => f
                .bin_op_lhs(power.into(), crate::ast::BinaryOp::Pow)
                .map(Into::into),
        }
    }

    pub fn inverse(self) -> Self {
        match self {
            Value::NegOne => Value::NegOne,
            Value::One => Value::One,
            Value::Zero => panic!("Can't invert zero!"),
            Value::Real(real) => Value::Real(real.inverse()),
            Value::Complex(complex) => Value::Complex(complex.inverse()),
            Value::Mat(_) => unimplemented!(),
            Value::Function(_) => panic!("Cannot take inverse of function!"),
        }
    }

    pub fn conjugate(self) -> Self {
        match self {
            Value::One | Value::Zero | Value::NegOne | Value::Real(_) => self,
            Value::Complex(x) => x.conj().into(),
            Value::Mat(x) => x.conj().into(),
            Value::Function(x) => x.unary_op(crate::ast::UnaryOp::Conj).into(),
        }
    }

    pub fn transpose(self) -> Self {
        match self {
            Value::One | Value::Zero | Value::NegOne | Value::Real(_) | Value::Complex(_) => self,
            Value::Mat(x) => x.transpose().into(),
            Value::Function(x) => x.unary_op(crate::ast::UnaryOp::Transpose).into(),
        }
    }

    pub fn norm(self) -> Self {
        match self {
            Value::One | Value::Zero | Value::NegOne | Value::Real(_) | Value::Complex(_) => self,
            Value::Mat(mat) => mat
                .norm_squared()
                .pow(Rational { num: 1, denom: 2 }.into())
                .expect("Could not take sqrt for norm!"),
            Value::Function(function) => function.unary_op(crate::ast::UnaryOp::Norm).into(),
        }
    }

    pub fn mag(self) -> Self {
        match self {
            Value::One | Value::Zero => self,
            Value::NegOne => Value::One,
            Value::Real(real) => real.abs().into(),
            Value::Complex(complex) => complex.mag_sq().sqrt().into(),
            Value::Mat(matrix) => matrix.det().into(),
            Value::Function(function) => function.unary_op(crate::ast::UnaryOp::Mag).into(),
        }
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

impl From<Matrix<Value>> for Value {
    fn from(value: Matrix<Value>) -> Self {
        Value::Mat(value)
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Function(value)
    }
}

impl From<i128> for Value {
    fn from(value: i128) -> Self {
        Rational::try_from(value).unwrap().into()
    }
}

impl Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::One, Value::NegOne) | (Value::NegOne, Value::One) => Value::Zero,
            (x, Value::Zero) | (Value::Zero, x) => x,

            (Value::Function(f), x) => f.bin_op_lhs(x, crate::ast::BinaryOp::Add).unwrap().into(),
            (x, Value::Function(f)) => f.bin_op_rhs(x, crate::ast::BinaryOp::Add).unwrap().into(),

            (Value::One, rhs @ (Value::Real(_) | Value::Complex(_)))
            | (rhs @ (Value::Real(_) | Value::Complex(_)), Value::One) => Value::from(1) + rhs,

            (Value::One, Value::Mat(rhs)) | (Value::Mat(rhs), Value::One) => {
                (rhs.clone() + rhs.identity()).into()
            }

            (Value::NegOne, Value::Mat(rhs)) => (rhs.identity() - rhs).into(),

            (Value::Mat(rhs), Value::NegOne) => (rhs.clone() - rhs.identity()).into(),

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
            (Value::One, Value::One) | (Value::NegOne, Value::NegOne) => Value::Zero,
            (x, Value::Zero) | (Value::Zero, x) => x,

            (Value::Function(f), x) => f.bin_op_lhs(x, crate::ast::BinaryOp::Sub).unwrap().into(),
            (x, Value::Function(f)) => f.bin_op_rhs(x, crate::ast::BinaryOp::Sub).unwrap().into(),

            (Value::One, rhs @ (Value::Real(_) | Value::Complex(_))) => {
                (Value::from(1) - rhs).into()
            }
            (rhs @ (Value::Real(_) | Value::Complex(_)), Value::One) => rhs - Value::from(1),

            (Value::One, Value::Mat(rhs)) => (rhs.identity() - rhs).into(),
            (Value::Mat(rhs), Value::One) => (rhs.clone() - rhs.identity()).into(),

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
            (Value::NegOne, Value::NegOne) => Value::One,
            (Value::One, x) => x,
            (x, Value::One) => x,
            (Value::Zero, _) | (_, Value::Zero) => Value::Zero,

            (Value::NegOne, x) | (x, Value::NegOne) => x.neg(),

            (Value::Real(x), Value::Real(y)) => (x * y).into(),
            (Value::Complex(x), Value::Real(y)) => (x * y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) * y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x * y).into(),

            (Value::Mat(x), Value::Mat(y)) => {
                let mut result = x * y;

                if result.cols == 1 && result.rows == 1 {
                    result.data.pop().unwrap().into()
                } else {
                    result.into()
                }
            }

            (Value::Mat(x), y) => x.mul_rhs(y).into(),
            (x, Value::Mat(y)) => y.mul_lhs(x).into(),

            (Value::Function(f), x) => f.bin_op_lhs(x, crate::ast::BinaryOp::Mul).unwrap().into(),
            (x, Value::Function(f)) => f.bin_op_rhs(x, crate::ast::BinaryOp::Mul).unwrap().into(),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::One, x) => x.inverse(),
            (x, Value::One) => x,
            (Value::Zero, _) => Value::Zero,

            (Value::NegOne, x) => x.inverse().neg(),
            (x, Value::NegOne) => x.neg(),

            (Value::Real(x), Value::Real(y)) => (x / y).into(),
            (Value::Complex(x), Value::Real(y)) => (x / y).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) / y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x / y).into(),

            (x, Value::Mat(y)) => y.inverse().mul_lhs(x).into(),
            (Value::Mat(x), y) => x.mul_rhs(y.inverse()).into(),

            (Value::Function(f), x) => f.bin_op_lhs(x, crate::ast::BinaryOp::Div).unwrap().into(),
            (x, Value::Function(f)) => f.bin_op_rhs(x, crate::ast::BinaryOp::Div).unwrap().into(),

            (x, y) => panic!("Division operation not supported between {x:?} and {y:?}"),
        }
    }
}

impl Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::One, x) => Value::from(1).rem(x),
            (x, Value::One) => x.rem(Value::from(1)),

            (Value::NegOne, x) => Value::from(-1).rem(x),
            (x, Value::NegOne) => x.rem(Value::from(-1)),

            (Value::Zero, _) => Value::Zero,

            (Value::Real(x), Value::Real(y)) => (x % y).into(),
            (Value::Complex(x), Value::Real(y)) => (x % Complex::from(y)).into(),
            (Value::Real(x), Value::Complex(y)) => (Complex::from(x) % y).into(),
            (Value::Complex(x), Value::Complex(y)) => (x % y).into(),

            _ => unimplemented!(),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::One => Value::NegOne,
            Value::NegOne => Value::One,
            Value::Zero => Value::Zero,

            Value::Real(x) => (-x).into(),
            Value::Mat(x) => (-x).into(),
            Value::Complex(x) => (-x).into(),
            Value::Function(f) => Function::CompositeUnary {
                term: f.into(),
                op: crate::ast::UnaryOp::Neg,
            }
            .into(),
        }
    }
}

impl Ring for Value {
    const ONE: Self = Value::One;
    const ZERO: Self = Value::Zero;
}
