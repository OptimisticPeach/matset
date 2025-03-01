use serde::{Deserialize, Serialize};

use super::{Field, Ring};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Complex<T: Ring> {
    real: T,
    imag: T,
}

impl<T: Ring> Default for Complex<T> {
    fn default() -> Self {
        Self {
            real: T::ZERO,
            imag: T::ZERO,
        }
    }
}

impl<T: Ring> Complex<T> {
    pub fn conj(self) -> Self {
        Self {
            imag: -self.imag,
            ..self
        }
    }

    pub fn mag_sq(self) -> T {
        self.real.clone() * self.real + self.imag.clone() * self.imag
    }

    pub fn parse(x: &str) -> Option<Self> {
        if x == "i" {
            return Self {
                real: T::ZERO,
                imag: T::ONE,
            }
            .into();
        }

        None
    }
}

impl<T: Ring> From<T> for Complex<T> {
    fn from(value: T) -> Self {
        Self {
            real: value,
            imag: T::ZERO,
        }
    }
}

impl<T: Ring> Add for Complex<T> {
    type Output = Complex<T>;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}

impl<T: Ring> Sub for Complex<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl<T: Ring> Mul for Complex<T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real.clone() * rhs.real.clone() - self.imag.clone() * rhs.imag.clone(),
            imag: self.real.clone() * rhs.imag.clone() + self.imag * rhs.real,
        }
    }
}

impl<T: Field> Div for Complex<T> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let num = self * rhs.clone().conj();
        let denom = rhs.mag_sq().inverse();

        Self {
            real: num.real * denom.clone(),
            imag: num.imag * denom,
        }
    }
}

impl<T: Ring> Neg for Complex<T> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            real: -self.real,
            imag: -self.imag,
        }
    }
}

impl<T: Ring> Ring for Complex<T> {
    const ONE: Self = Self {
        real: T::ONE,
        imag: T::ZERO,
    };

    const ZERO: Self = Self {
        real: T::ZERO,
        imag: T::ZERO,
    };
}

impl<T: Field> Field for Complex<T> {
    fn inverse(self) -> Self {
        let conj = self.clone().conj();
        let mag = self.mag_sq().inverse();

        Self {
            real: conj.real * mag.clone(),
            imag: conj.imag * mag,
        }
    }
}

impl<T: Ring> Add<T> for Complex<T> {
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        self.add(Self::from(rhs))
    }
}

impl<T: Ring> Sub<T> for Complex<T> {
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        self.sub(Self::from(rhs))
    }
}

impl<T: Ring> Mul<T> for Complex<T> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        self.mul(Self::from(rhs))
    }
}

impl<T: Field> Div<T> for Complex<T> {
    type Output = Complex<T>;

    fn div(self, rhs: T) -> Self::Output {
        self.div(Self::from(rhs))
    }
}
