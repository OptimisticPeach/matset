use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use super::{Field, Ring, complex::Complex, mat::Matrix};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Rational {
    pub(crate) num: i128,
    pub(crate) denom: u128,
}

impl Rational {
    fn unum(&self) -> u128 {
        self.num.abs() as u128
    }
}

fn gcd(a: u128, b: u128) -> u128 {
    let mut old_q = 0;

    let mut q = a;
    let mut r = b;

    while r != 0 {
        old_q = q;

        let modulo = q % r;
        q = r;
        r = modulo;
    }

    old_q
}

impl Add for Rational {
    type Output = Rational;

    fn add(self, rhs: Self) -> Self::Output {
        let denom_gcd = gcd(self.denom, rhs.denom);

        let v1d = self.denom / denom_gcd;
        let v2d = rhs.denom / denom_gcd;

        let new_denom = v1d * rhs.denom;
        let new_num = self.num * v2d as i128 + rhs.num * v1d as i128;

        let gcd = gcd(new_num.abs() as u128, new_denom);

        Self {
            num: new_num / gcd as i128,
            denom: new_denom / gcd,
        }
    }
}

impl Sub for Rational {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Rational {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let gcd_ad = gcd(self.unum(), rhs.denom);
        let gcd_bc = gcd(self.denom, rhs.unum());

        let num = (self.num / gcd_ad as i128) * (rhs.num / gcd_bc as i128);
        let denom = (self.denom / gcd_bc) * (rhs.denom / gcd_ad);

        Self { num, denom }
    }
}

impl Div for Rational {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.inverse()
    }
}

impl Neg for Rational {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            num: -self.num,
            ..self
        }
    }
}

impl AddAssign for Rational {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl SubAssign for Rational {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl MulAssign for Rational {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs
    }
}

impl DivAssign for Rational {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs
    }
}

impl Ring for Rational {
    const ONE: Self = Self { num: 1, denom: 1 };

    const ZERO: Self = Self { num: 0, denom: 1 };
}

impl Field for Rational {
    fn inverse(self) -> Self {
        if self.denom == 0 {
            panic!("Cannot invert {self:?}!");
        }

        Self {
            num: self.denom as i128 * self.num.signum(),
            denom: self.unum(),
        }
    }
}

impl Add<Complex> for Rational {
    type Output = Complex;

    fn add(self, rhs: Complex) -> Self::Output {
        Complex::from(self) + rhs
    }
}

impl Sub<Complex> for Rational {
    type Output = Complex;

    fn sub(self, rhs: Complex) -> Self::Output {
        Complex::from(self) - rhs
    }
}

impl Mul<Complex> for Rational {
    type Output = Complex;

    fn mul(self, rhs: Complex) -> Self::Output {
        Complex::from(self) * rhs
    }
}

impl Div<Complex> for Rational {
    type Output = Complex;

    fn div(self, rhs: Complex) -> Self::Output {
        Complex::from(self) / rhs
    }
}

impl<T: Ring + Clone> Mul<Matrix<T>> for Rational
where
    Self: Mul<T, Output = T>,
{
    type Output = Matrix<T>;

    fn mul(self, rhs: Matrix<T>) -> Self::Output {
        Matrix {
            data: rhs.data.into_iter().map(|x| self * x).collect(),
            ..rhs
        }
    }
}
