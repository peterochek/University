use std::io;

const MOD: i64 = 998_244_353;
const MAX: usize = 101;

fn gcd(a: i64, b: i64, x: &mut i64, y: &mut i64) -> i64 {
    if a == 0 {
        *x = 0;
        *y = 1;
        return b;
    }
    let mut x1 = 0;
    let mut y1 = 0;
    let d = gcd(b % a, a, &mut x1, &mut y1);
    *x = y1 - ((b / a) * x1);
    *y = x1;
    d
}

fn gcd_emaxx(a: i64) -> i64 {
    let (mut x, mut y) = (0, 0);
    let g = gcd(a, MOD, &mut x, &mut y);
    if g != 1 {
        1
    } else {
        (x % MOD + MOD) % MOD
    }
}

fn exp() -> Vec<i64> {
    let mut coefs = vec![0; MAX + 1];
    coefs[0] = 1;
    for i in 1..=MAX {
        coefs[i] = (coefs[i - 1] * gcd_emaxx(i as i64)) % MOD;
    }
    coefs
}

fn log() -> Vec<i64> {
    let mut coefs = vec![0; MAX + 1];
    coefs[1] = 1;
    for i in 2..=MAX {
        coefs[i] =
            (((-coefs[i - 1] % MOD + MOD) * (i as i64 - 1)) % MOD * gcd_emaxx(i as i64)) % MOD;
    }
    coefs
}

fn sqrt() -> Vec<i64> {
    let mut coefs = vec![0; MAX + 1];
    coefs[0] = 1;
    for i in 1..=MAX {
        coefs[i] = (((-coefs[i - 1] % MOD + MOD)
            * (((2 * i - 1) as i64 * (2 * i) as i64 * (3_i64 - 2 * i as i64)) + MOD)
            % MOD)
            % MOD
            * gcd_emaxx((i * i * 4) as i64 * (1_i64 - 2 * i as i64) + MOD))
            % MOD;
    }
    coefs
}

fn mul(p: &[i64], q: &[i64]) -> Vec<i64> {
    let mut res = vec![0; MAX];
    for i in 0..res.len() {
        for j in 0..=i {
            res[i] = (res[i] + (p[j] * q[i - j] % MOD)) % MOD;
        }
    }
    res
}

fn print(m: usize, mul_coef: &[i64], coefs: &[Vec<i64>]) {
    for i in 0..m {
        let mut coef = 0;
        for j in 0..m {
            coef = (coef + mul_coef[j] * coefs[j][i] % MOD) % MOD;
        }
        print!("{} ", coef);
    }
    println!();
}

pub fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let inputs = input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<usize>>();
    let n = inputs[0];
    let m = inputs[1];

    let mut p = vec![0i64; MAX];
    let mut e = vec![0i64; MAX];
    e[0] = 1;

    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let inputs = input.split_whitespace().collect::<Vec<_>>();
    for i in 0..=n {
        p[i] = inputs[i].parse().unwrap();
    }

    let sqrt = sqrt();
    let exp = exp();
    let log = log();

    let mut coefs = vec![e, p.clone()];

    for i in 2..MAX {
        coefs.push(mul(&p, &coefs[i - 1]));
    }

    print(m, &sqrt, &coefs);
    print(m, &exp, &coefs);
    print(m, &log, &coefs);
}
