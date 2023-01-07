use num::{
    rational::Ratio, Bounded, Complex, Float, FromPrimitive, Num, NumCast, One, ToPrimitive, Zero,
};
use std::fmt;
use std::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Complex(Complex<f64>),
    ExactComplex(Complex<Ratio<i64>>),
    Real(f64),
    Rational(Ratio<i64>),
    Integer(i64),
}

impl Number {
    pub const ZERO: Self = Self::Integer(0);
    pub const ONE: Self = Self::Integer(1);
    pub const MIN: Self = Self::Integer(i64::MIN);
    pub const MAX: Self = Self::Integer(i64::MAX);
}

impl From<i64> for Number {
    fn from(n: i64) -> Self {
        Self::Integer(n)
    }
}

impl From<Ratio<i64>> for Number {
    fn from(n: Ratio<i64>) -> Self {
        Self::Rational(n)
    }
}

impl From<f64> for Number {
    fn from(n: f64) -> Self {
        Self::Real(n)
    }
}

impl From<Complex<f64>> for Number {
    fn from(n: Complex<f64>) -> Self {
        Self::Complex(n)
    }
}

impl From<Complex<Ratio<i64>>> for Number {
    fn from(n: Complex<Ratio<i64>>) -> Self {
        Self::ExactComplex(n)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Complex(n) => write!(f, "{}", n),
            Self::ExactComplex(n) => write!(f, "{}", n),
            Self::Real(n) => write!(f, "{}", n),
            Self::Rational(n) => write!(f, "{}", n),
            Self::Integer(n) => write!(f, "{}", n),
        }
    }
}

fn to_real(n: Ratio<i64>) -> f64 {
    *n.numer() as f64 / *n.denom() as f64
}

macro_rules! impl_op {
    ($self:ident, $rhs:ident, $op:tt) => {
        match ($self, $rhs) {
            (Self::Complex(n), Self::Complex(m)) => Self::Complex(n $op m),
            (Self::ExactComplex(n), Self::Complex(m)) => {
                Self::Complex(Complex::new(to_real(n.re), to_real(n.im)) $op m)
            }
            (Self::Real(n), Self::Complex(m)) => Self::Complex(n $op m),
            (Self::Rational(n), Self::Complex(m)) => Self::Complex(to_real(n) $op m),
            (Self::Integer(n), Self::Complex(m)) => Self::Complex(n as f64 $op m),
            (Self::Complex(n), Self::ExactComplex(m)) => {
                Self::Complex(n $op Complex::new(to_real(m.re), to_real(m.im)))
            }
            (Self::ExactComplex(n), Self::ExactComplex(m)) => Self::ExactComplex(n $op m),
            (Self::Real(n), Self::ExactComplex(m)) => {
                Self::Complex(Complex::new(to_real(m.re), to_real(m.im)) $op n)
            }
            (Self::Rational(n), Self::ExactComplex(m)) => Self::ExactComplex(m $op n),
            (Self::Integer(n), Self::ExactComplex(m)) => {
                Self::ExactComplex(m $op Ratio::from_integer(n))
            }
            (Self::Complex(n), Self::Real(m)) => Self::Complex(n $op m),
            (Self::ExactComplex(n), Self::Real(m)) => {
                Self::Complex(Complex::new(to_real(n.re), to_real(n.im)) $op m)
            }
            (Self::Real(n), Self::Real(m)) => Self::Real(n $op m),
            (Self::Rational(n), Self::Real(m)) => Self::Real(to_real(n) $op m),
            (Self::Integer(n), Self::Real(m)) => Self::Real(n as f64 $op m),
            (Self::Complex(n), Self::Rational(m)) => Self::Complex(n $op to_real(m)),
            (Self::ExactComplex(n), Self::Rational(m)) => Self::ExactComplex(n $op m),
            (Self::Real(n), Self::Rational(m)) => Self::Real(n $op to_real(m)),
            (Self::Rational(n), Self::Rational(m)) => Self::Rational(n $op m),
            (Self::Integer(n), Self::Rational(m)) => Self::Rational(m $op n),
            (Self::Complex(n), Self::Integer(m)) => {
                Self::Complex(n $op <Complex<f64> as From<f64>>::from(m as f64))
            }
            (Self::ExactComplex(n), Self::Integer(m)) => {
                Self::ExactComplex(n $op Ratio::from_integer(m))
            }
            (Self::Real(n), Self::Integer(m)) => Self::Real(n $op m as f64),
            (Self::Rational(n), Self::Integer(m)) => Self::Rational(n $op m),
            (Self::Integer(n), Self::Integer(m)) => Self::Integer(n $op m),
        }
    }
}

impl Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        impl_op!(self, rhs, +)
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        impl_op!(self, rhs, -)
    }
}

impl SubAssign for Number {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Mul for Number {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        impl_op!(self, rhs, *)
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        impl_op!(self, rhs, /)
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl Rem for Number {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        impl_op!(self, rhs, %)
    }
}

impl RemAssign for Number {
    fn rem_assign(&mut self, rhs: Self) {
        *self = *self % rhs;
    }
}

impl Neg for Number {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Self::Complex(n) => Self::Complex(-n),
            Self::ExactComplex(n) => Self::ExactComplex(-n),
            Self::Real(n) => Self::Real(-n),
            Self::Rational(n) => Self::Rational(-n),
            Self::Integer(n) => Self::Integer(-n),
        }
    }
}

macro_rules! each {
    ($self:ident, $method:ident) => {
        match $self {
            Self::Complex(n) => n.$method(),
            Self::ExactComplex(n) => n.$method(),
            Self::Real(n) => n.$method(),
            Self::Rational(n) => n.$method(),
            Self::Integer(n) => n.$method(),
        }
    };
}

impl Zero for Number {
    fn zero() -> Self {
        Self::ZERO
    }
    fn is_zero(&self) -> bool {
        each!(self, is_zero)
    }
    fn set_zero(&mut self) {
        each!(self, set_zero)
    }
}

impl One for Number {
    fn one() -> Self {
        Self::ONE
    }
    fn is_one(&self) -> bool {
        each!(self, is_one)
    }
    fn set_one(&mut self) {
        each!(self, set_one)
    }
}

impl FromPrimitive for Number {
    fn from_i64(n: i64) -> Option<Self> {
        Some(Self::Integer(n))
    }
    fn from_u64(n: u64) -> Option<Self> {
        Some(Self::Integer(n as i64))
    }
    fn from_f32(n: f32) -> Option<Self> {
        Some(Self::Real(n as f64))
    }
    fn from_f64(n: f64) -> Option<Self> {
        Some(Self::Real(n))
    }
}

impl Bounded for Number {
    fn min_value() -> Self {
        Self::MIN
    }
    fn max_value() -> Self {
        Self::MAX
    }
}
