use std::io::stdin;
use std::ops::{Add, Index, Mul};

const MOD: i64 = 998_244_353;

struct GF {
    coefficients: Vec<i64>,
    size: usize,
}

impl GF {
    fn new() -> Self {
        Self {
            coefficients: Vec::new(),
            size: 0,
        }
    }

    fn with_capacity(size: usize) -> Self {
        Self {
            coefficients: vec![0; size + 1],
            size,
        }
    }
}

impl Index<usize> for GF {
    type Output = i64;
    fn index<'a>(&self, i: usize) -> &i64 {
        if i > self.size {
            &0
        } else {
            &self.coefficients[i]
        }
    }
}

impl<'a, 'b> Add<&'b GF> for &'a GF {
    type Output = GF;
    fn add(self, other: &'b GF) -> GF {
        let mut ret = GF::new();
        ret.size = self.size.max(other.size);

        for i in 0..=ret.size {
            let coef = (self[i] + other[i]) % MOD;
            ret.coefficients.push(coef);
        }

        ret
    }
}

impl<'a, 'b> Mul<&'b GF> for &'a GF {
    type Output = GF;
    fn mul(self, other: &'b GF) -> GF {
        let mut ret = GF::new();
        ret.size = self.size + other.size;

        for i in 0..=ret.size {
            let mut mul = 0;
            for j in 0..=i {
                mul = (mul + self[j] * other[i - j]) % MOD;
            }
            ret.coefficients.push(mul);
        }

        ret
    }
}

pub fn main() {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let mut iter = input.split_whitespace();
    let n: usize = iter.next().unwrap().parse().unwrap();
    let m: usize = iter.next().unwrap().parse().unwrap();

    input.clear();
    stdin().read_line(&mut input).unwrap();
    let mut iter = input.split_whitespace();
    let mut p = GF::with_capacity(n);
    for i in 0..=n {
        p.coefficients[i] = iter.next().unwrap().parse().unwrap();
    }

    input.clear();
    stdin().read_line(&mut input).unwrap();
    let mut iter = input.split_whitespace();
    let mut q = GF::with_capacity(m);
    for i in 0..=m {
        q.coefficients[i] = iter.next().unwrap().parse().unwrap();
    }

    let mut one_over_q = GF::new();
    one_over_q.size = 1000;
    one_over_q.coefficients.push(1 / q[0] % MOD);
    for i in 1..=one_over_q.size {
        let mut coefficient = 0;
        for j in 1..=i {
            coefficient = (coefficient + q[j] * one_over_q[i - j]) % MOD;
        }
        one_over_q
            .coefficients
            .push((-coefficient / one_over_q[0] + MOD) % MOD);
    }

    let p_sum_q = &p + &q;
    let p_mul_q = &p * &q;

    println!("{}", p_sum_q.size);
    for i in 0..=p_sum_q.size {
        print!("{} ", p_sum_q[i]);
    }
    println!();

    println!("{}", p_mul_q.size);
    for i in 0..=p_mul_q.size {
        print!("{} ", p_mul_q[i]);
    }
    println!();

    for i in 0..one_over_q.size {
        let mut coef = 0;
        for j in 0..=i {
            coef = (coef + p[j] * one_over_q[i - j] + MOD) % MOD;
        }
        print!("{} ", coef);
    }
}
