use std::ops::{Add, AddAssign, Index, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use super::{Ring, complex::Complex, rational::Rational};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Matrix<T: Ring + Clone> {
    pub rows: u8,
    pub cols: u8,

    /// Data is arranged as [[T; rows]; cols], i.e. column major.
    pub data: Vec<T>,
}

impl<T: Ring + Clone> Default for Matrix<T> {
    fn default() -> Self {
        Self {
            rows: 0,
            cols: 0,
            data: vec![],
        }
    }
}

impl<T: Ring + Clone> Index<(u8, u8)> for Matrix<T> {
    type Output = T;

    fn index(&self, (col, row): (u8, u8)) -> &Self::Output {
        let idx = col as usize * self.rows as usize + row as usize;

        &self.data[idx]
    }
}

impl<T: Ring + Clone> Matrix<T> {
    fn confirm_add_size(&self, other: &Self) {
        if self.rows != other.rows || self.cols != other.cols {
            panic!(
                "Matrices of sizes ({}, {}) and ({}, {}) cannot be added!",
                self.rows, self.cols, other.rows, other.cols
            );
        }
    }

    fn confirm_mul_size(&self, other: &Self) {
        if self.cols != other.rows {
            panic!(
                "Matrices of sizes ({}, {}) and ({}, {}) cannot be multiplied!",
                self.rows, self.cols, other.rows, other.cols
            )
        }
    }
}

impl<T: Ring + Clone> Add for Matrix<T> {
    type Output = Matrix<T>;

    fn add(self, rhs: Self) -> Self::Output {
        if self.cols == 0 && self.rows == 0 {
            return rhs;
        }
        if rhs.cols == 0 && rhs.rows == 0 {
            return self;
        }

        self.confirm_add_size(&rhs);

        Self {
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(x, y)| x + y)
                .collect(),
            ..self
        }
    }
}

impl<T: Ring + Clone> Sub for Matrix<T> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.cols == 0 && self.rows == 0 {
            return rhs;
        }
        if rhs.cols == 0 && rhs.rows == 0 {
            return self;
        }
        self.confirm_add_size(&rhs);

        Self {
            data: self
                .data
                .into_iter()
                .zip(rhs.data.into_iter())
                .map(|(x, y)| x - y)
                .collect(),
            ..self
        }
    }
}

impl<T: Ring + Clone> Mul for Matrix<T> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.rows == 0 && self.cols == 0 {
            return rhs;
        }
        if rhs.rows == 0 && rhs.cols == 0 {
            return self;
        }

        self.confirm_mul_size(&rhs);

        let (cols, mid, rows) = (rhs.cols, rhs.rows, self.rows);

        let data = (0..cols)
            .flat_map(|col| (0..rows).map(move |row| (col, row)))
            .map(|(col, row)| {
                (0..mid)
                    .map(|mid| self[(mid, row)].clone() * rhs[(col, mid)].clone())
                    .reduce(Add::add)
                    .unwrap()
            })
            .collect();

        Self { cols, rows, data }
    }
}

impl<T: Ring + Clone> Neg for Matrix<T> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            data: self.data.into_iter().map(Neg::neg).collect(),
            ..self
        }
    }
}

impl<T: Ring + Clone> AddAssign for Matrix<T> {
    fn add_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) + rhs
    }
}

impl<T: Ring + Clone> SubAssign for Matrix<T> {
    fn sub_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) - rhs
    }
}

impl<T: Ring + Clone> MulAssign for Matrix<T> {
    fn mul_assign(&mut self, rhs: Self) {
        *self = std::mem::take(self) * rhs
    }
}

impl<T: Ring + Clone> Ring for Matrix<T> {
    const ONE: Self = Self {
        rows: 0,
        cols: 0,
        data: vec![],
    };

    const ZERO: Self = Self {
        rows: 0,
        cols: 0,
        data: vec![],
    };
}

impl<T: Ring + Clone> Mul<Rational> for Matrix<T>
where
    T: Mul<Rational>,
    <T as Mul<Rational>>::Output: Ring + Clone,
{
    type Output = Matrix<<T as Mul<Rational>>::Output>;

    fn mul(self, rhs: Rational) -> Self::Output {
        Matrix {
            data: self.data.into_iter().map(|x| x * rhs).collect(),
            rows: self.rows,
            cols: self.cols,
        }
    }
}

impl<T: Ring + Clone> Mul<Complex> for Matrix<T>
where
    T: Mul<Complex>,
    <T as Mul<Complex>>::Output: Ring + Clone,
{
    type Output = Matrix<<T as Mul<Complex>>::Output>;

    fn mul(self, rhs: Complex) -> Self::Output {
        Matrix {
            data: self.data.into_iter().map(|x| x * rhs).collect(),
            rows: self.rows,
            cols: self.cols,
        }
    }
}
