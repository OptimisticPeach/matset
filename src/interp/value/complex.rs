use serde::{Deserialize, Serialize};

use super::mat::Matrix;
use super::rational::Rational;
use super::{Field, Ring};
use std::fmt::Display;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Complex {
    real: Rational,
    imag: Rational,
}

impl Complex {
    pub fn conj(self) -> Self {
        Self {
            imag: -self.imag,
            ..self
        }
    }

    pub fn mag_sq(self) -> Rational {
        self.real * self.real + self.imag * self.imag
    }

    pub fn parse(x: &str) -> Option<Self> {
        if x == "i" {
            return Self {
                real: Rational::ZERO,
                imag: Rational::ONE,
            }
            .into();
        }

        None
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::ZERO {
            return write!(f, "0");
        }

        if self.real == Rational::ZERO {
            write!(f, "{}i", self.imag)
        } else if self.imag == Rational::ZERO {
            write!(f, "{}", self.real)
        } else {
            write!(f, "{} + {}i", self.real, self.imag)
        }
    }
}

impl From<Rational> for Complex {
    fn from(value: Rational) -> Self {
        Self {
            real: value,
            imag: Rational::ZERO,
        }
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}

impl Sub for Complex {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real * rhs.real - self.imag * rhs.imag,
            imag: self.real * rhs.imag + self.imag * rhs.real,
        }
    }
}

impl Div for Complex {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let num = self * rhs.conj();
        let denom = rhs.mag_sq().inverse();

        Self {
            real: num.real * denom,
            imag: num.imag * denom,
        }
    }
}

impl Neg for Complex {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            real: -self.real,
            imag: -self.imag,
        }
    }
}

impl AddAssign for Complex {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl SubAssign for Complex {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl MulAssign for Complex {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs
    }
}

impl DivAssign for Complex {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs
    }
}

impl Ring for Complex {
    const ONE: Self = Self {
        real: Rational::ONE,
        imag: Rational::ZERO,
    };

    const ZERO: Self = Self {
        real: Rational::ZERO,
        imag: Rational::ZERO,
    };
}

impl Field for Complex {
    fn inverse(self) -> Self {
        let conj = self.conj();
        let mag = self.mag_sq().inverse();

        Self {
            real: conj.real * mag,
            imag: conj.imag * mag,
        }
    }
}

impl Add<Rational> for Complex {
    type Output = Complex;

    fn add(self, rhs: Rational) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl Sub<Rational> for Complex {
    type Output = Complex;

    fn sub(self, rhs: Rational) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl Mul<Rational> for Complex {
    type Output = Complex;

    fn mul(self, rhs: Rational) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl Div<Rational> for Complex {
    type Output = Complex;

    fn div(self, rhs: Rational) -> Self::Output {
        self.div(Self::from(rhs))
    }
}

impl AddAssign<Rational> for Complex {
    fn add_assign(&mut self, rhs: Rational) {
        *self = *self + rhs
    }
}

impl SubAssign<Rational> for Complex {
    fn sub_assign(&mut self, rhs: Rational) {
        *self = *self - rhs
    }
}

impl MulAssign<Rational> for Complex {
    fn mul_assign(&mut self, rhs: Rational) {
        *self = *self * rhs
    }
}

impl DivAssign<Rational> for Complex {
    fn div_assign(&mut self, rhs: Rational) {
        *self = *self / rhs
    }
}

impl<T: Ring + Clone> Mul<Matrix<T>> for Complex
where
    Complex: Mul<T, Output = T>,
{
    type Output = Matrix<T>;

    fn mul(self, rhs: Matrix<T>) -> Self::Output {
        Matrix {
            data: rhs.data.into_iter().map(|x| self * x).collect(),
            ..rhs
        }
    }
}
