use std::ops::{Add, Div, Mul, Neg, Sub};

use serde::{Deserialize, Serialize};

use super::{Field, Ring, mat::Matrix};

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Rational {
    pub(crate) num: i128,
    pub(crate) denom: u128,
}

impl Rational {
    fn unum(&self) -> u128 {
        self.num.abs() as u128
    }

    pub fn parse(x: &str) -> Option<Self> {
        if let Ok(x) = x.parse::<i128>() {
            return Some(Rational { num: x, denom: 1 });
        }

        // only case where it could've not parsed is if
        // we got a decimal like 1.2

        let mut split = x.split(".");
        let integer = split.next()?;
        let fractional = split.next()?;

        if fractional.len() > 6 {
            return None;
        }

        if split.next() != None {
            return None;
        }

        let integer = integer.parse::<i128>().ok()?;

        let fractional_len = fractional.len();
        let fractional = fractional.parse::<u128>().ok()?;

        let fract = Self {
            num: fractional as i128,
            denom: 1,
        } / Self::from((10u128).pow(fractional_len as u32));

        Some(Self::from(integer) + fract)
    }
}

fn gcd(mut a: u128, mut b: u128) -> u128 {
    while b != 0 {
        let t = b;

        b = a % b;

        a = t;
    }

    a
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

impl<T: TryInto<i128>> From<T> for Rational {
    fn from(value: T) -> Self {
        Self {
            num: value.try_into().ok().expect("Number too big!"),
            denom: 1,
        }
    }
}
