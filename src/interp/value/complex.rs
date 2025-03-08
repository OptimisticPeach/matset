use serde::{Deserialize, Serialize};

use super::{Field, Ring, rational::Rational, real::Real};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Complex<T: Ring> {
    pub real: T,
    pub imag: T,
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
    pub const I: Self = Self {
        real: T::ZERO,
        imag: T::ONE,
    };

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

impl Complex<Real> {
    pub fn floatify(&mut self) {
        self.real.floatify();
        self.imag.floatify();
    }

    pub fn floor(self) -> Self {
        let result = Self {
            real: self.real.floor(),
            imag: self.imag.floor(),
        };

        let x = self.real.fract();
        let y = self.imag.fract();

        if Real::ONE <= x + y {
            if x >= y {
                result + Complex::ONE
            } else {
                result + Complex::I
            }
        } else {
            result
        }
    }

    pub fn is_zero(&self) -> bool {
        self.real.is_zero() && self.imag.is_zero()
    }

    pub fn sqrt(self) -> Self {
        self.pow(Complex::from(Real::from(0.5f64)))
    }

    pub fn pow(self, other: Self) -> Self {
        if self.is_zero() {
            Self::ZERO
        } else if let Complex {
            real: Real::Rational(Rational { num, denom: 1 }),
            imag: Real::Rational(Rational { num: 0, denom: 1 }),
        } = other
        {
            let mut pow = num.abs() as u128;
            let mut acc = Self::ONE;

            let mut shift = if num < 0 { self.inverse() } else { self };

            while pow != 0 {
                if pow & 1 != 0 {
                    acc = acc * shift;
                }

                shift = shift * shift;
                pow >>= 1;
            }

            return acc;
        } else {
            let x = self.real.to_f64();
            let y = self.imag.to_f64();

            let mag = x.hypot(y).ln();
            let arg = y.atan2(x);

            let a = other.real.to_f64();
            let b = other.imag.to_f64();

            let real = mag * a - arg * b;
            let imag = mag * b + arg * a;

            let real = real.exp();
            let (s, c) = imag.sin_cos();

            let x = c * real;
            let y = s * real;

            Complex {
                real: Real::Float(x),
                imag: Real::Float(y),
            }
        }
    }

    pub fn exp(self) -> Self {
        let x = self.real.to_f64();
        let y = self.imag.to_f64();

        let scl = x.exp();
        let (s, c) = y.sin_cos();

        Complex {
            real: Real::Float(scl * c),
            imag: Real::Float(scl * s),
        }
    }

    pub fn sin(self) -> Self {
        let Self {
            real: Real::Float(re),
            imag: Real::Float(im),
        } = self.muli().exp() - self.muli().neg().exp()
        else {
            unreachable!()
        };

        Self {
            real: Real::Float(re / 2.0),
            imag: Real::Float(im / 2.0),
        }
        .muli()
        .neg()
    }

    pub fn cos(self) -> Self {
        let Self {
            real: Real::Float(re),
            imag: Real::Float(im),
        } = self.muli().exp() + self.muli().neg().exp()
        else {
            unreachable!()
        };

        Self {
            real: Real::Float(re / 2.0),
            imag: Real::Float(im / 2.0),
        }
    }

    pub fn tan(self) -> Self {
        self.sin() / self.cos()
    }

    pub fn sinh(self) -> Self {
        let Self {
            real: Real::Float(re),
            imag: Real::Float(im),
        } = self.exp() - self.neg().exp()
        else {
            unreachable!()
        };

        Self {
            real: Real::Float(re / 2.0),
            imag: Real::Float(im / 2.0),
        }
    }

    pub fn cosh(self) -> Self {
        let Self {
            real: Real::Float(re),
            imag: Real::Float(im),
        } = self.exp() + self.neg().exp()
        else {
            unreachable!()
        };

        Self {
            real: Real::Float(re / 2.0),
            imag: Real::Float(im / 2.0),
        }
    }

    pub fn tanh(self) -> Self {
        self.sinh() / self.cosh()
    }

    pub fn ln(self) -> Self {
        if self == Self::ONE {
            return Self::ZERO;
        }

        let x = self.real.to_f64();
        let y = self.imag.to_f64();

        let mag = x.hypot(y).ln();
        let arg = y.atan2(x);

        Self {
            real: Real::Float(mag),
            imag: Real::Float(arg),
        }
    }

    pub fn muli(self) -> Self {
        Self {
            real: -self.imag,
            imag: self.real,
        }
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

impl Rem for Complex<Real> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        self - rhs * (self / rhs).floor()
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
