use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

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

    pub fn fract(self) -> Self {
        Self {
            num: self.num % self.denom as i128,
            denom: self.denom,
        }
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

    fn powi(mut self, mut power: u128) -> Self {
        let mut acc = Self::ONE;

        while power != 0 {
            if power & 1 == 1 {
                acc = acc * self;
            }

            self = self * self;
            power >>= 1;
        }

        acc
    }

    pub fn pow(mut self, other: Self) -> Option<Self> {
        self = self.powi(other.num.abs() as u128);

        if other.num < 0 {
            self = self.inverse();
        }

        if other.denom == 1 {
            return Some(self);
        }

        if self.num < 0 && other.denom % 2 == 0 {
            return None;
        }

        let num_root = (self.num as f64).powf(1.0 / other.denom as f64).round() as i128;

        if num_root.pow(other.denom as u32) != self.num {
            return None;
        }

        let denom_root = (self.denom as f64).powf(1.0 / other.denom as f64).round() as u128;

        if denom_root.pow(other.denom as u32) != self.denom {
            return None;
        }

        Some(Self {
            num: num_root,
            denom: denom_root,
        })
    }

    pub fn is_zero(&self) -> bool {
        self.num == 0
    }

    pub fn floor(self) -> Self {
        Self::from(self.num / self.denom as i128)
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

impl Rem for Rational {
    type Output = Rational;

    fn rem(self, rhs: Self) -> Rational {
        // x = qy + r
        // x / y = q + r/y, q in Z, |r/y| < 1
        let rem_div_rhs = (self / rhs).fract();

        rem_div_rhs * rhs
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

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // a/b ~ c/d <==> ad ~ bc
        (self.num * other.denom as i128).partial_cmp(&(other.num * self.denom as i128))
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
