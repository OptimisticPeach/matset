use std::{
    fmt::Display,
    ops::{Add, Index, Mul, Neg, Sub},
};

use serde::{Deserialize, Serialize};

use super::{Ring, complex::Complex, real::Real};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Matrix<T: Ring + Clone> {
    pub rows: u8,
    pub cols: u8,

    /// Data is arranged as [[T; rows]; cols], i.e. column major.
    pub data: Vec<T>,
}

impl<T: Ring> Matrix<T> {
    pub fn mul_lhs<U: Ring>(self, lhs: U) -> Matrix<<U as Mul<T>>::Output>
    where
        U: Mul<T>,
        <U as Mul<T>>::Output: Ring,
    {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            data: self
                .data
                .into_iter()
                .map(|x| lhs.clone() * x)
                .collect::<Vec<<U as Mul<T>>::Output>>(),
        }
    }

    pub fn mul_rhs<U: Ring>(self, rhs: U) -> Matrix<<T as Mul<U>>::Output>
    where
        T: Mul<U>,
        <T as Mul<U>>::Output: Ring,
    {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            data: self
                .data
                .into_iter()
                .map(|x| x * rhs.clone())
                .collect::<Vec<<T as Mul<U>>::Output>>(),
        }
    }

    fn det_inner(&self, row_level: u8, columns: &mut Vec<u8>) -> T {
        if columns.len() == 2 {
            return self[(row_level, columns[0])].clone()
                * self[(row_level + 1, columns[1])].clone()
                - self[(row_level, columns[1])].clone()
                    * self[(row_level + 1, columns[0])].clone();
        }

        let mut acc = T::ZERO;

        for i in 0..columns.len() {
            let column = columns.remove(i);

            let elem = self[(row_level, column)].clone();

            let d = self.det_inner(row_level + 1, columns);

            if i % 2 == 0 {
                acc = acc + elem * d;
            } else {
                acc = acc - elem * d;
            }

            columns.insert(i, column);
        }

        acc
    }

    pub fn det(&self) -> T {
        if self.rows != self.cols {
            panic!("To take determinant, matrix must be square!")
        }

        let mut cols = (0..self.cols).collect::<Vec<_>>();

        self.det_inner(0, &mut cols)
    }

    pub fn trace(&self) -> T {
        if self.rows != self.cols {
            panic!("To take determinant, matrix must be square!")
        }

        let mut acc = T::ZERO;

        for i in 0..self.cols {
            acc = acc + self[(i, i)].clone();
        }

        acc
    }

    pub fn transpose(mut self) -> Self {
        let mut result = Vec::with_capacity(self.rows as usize * self.cols as usize);

        for row in 0..self.rows {
            for col in 0..self.cols {
                result.push(self[(col, row)].clone())
            }
        }

        std::mem::swap(&mut self.rows, &mut self.cols);
        std::mem::swap(&mut self.data, &mut result);

        self
    }
}

impl Matrix<Complex<Real>> {
    pub fn floatify(&mut self) {
        self.data.iter_mut().for_each(Complex::<Real>::floatify);
    }

    pub fn norm(&self) -> Complex<Real> {
        let result = self.clone().conj_transpose() * self.clone();

        assert!(result.rows == 1, "Cannot take norm of non vector");
        assert!(result.cols == 1, "Cannot take norm of non vector");

        result.data[0].sqrt()
    }
}

impl<T: Ring> Matrix<Complex<T>> {
    pub fn conj(mut self) -> Self {
        self.data.iter_mut().for_each(|x| *x = x.clone().conj());

        self
    }

    pub fn conj_transpose(self) -> Self {
        self.conj().transpose()
    }
}

impl<T: Ring + Clone + Display> Display for Matrix<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;

        let (rows, cols) = (self.rows as usize, self.cols as usize);

        for row in 0..rows {
            for col in 0..cols {
                write!(f, "{}", &self.data[col * rows + row])?;
                if col != cols - 1 {
                    write!(f, ", ")?;
                }
            }

            if row != rows - 1 {
                write!(f, "; ")?;
            }
        }

        write!(f, ")")
    }
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
