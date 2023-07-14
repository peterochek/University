use std::io;
use std::ops::{Index, IndexMut};

const MOD: i64 = 1_000_000_007;

#[derive(Default)]
struct GF {
    coefficients: Vec<i64>,
    size: usize,
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

impl IndexMut<usize> for GF {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.coefficients[index]
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let inputs = input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<usize>>();
    let _ = inputs[0];
    let m = inputs[1];

    let mut weights = vec![false; m + 1];
    let mut counts = GF::default();
    counts.size = m + 1;
    counts.coefficients = vec![0; counts.size];
    counts[0] = 1;

    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .for_each(|el: usize| weights[el] = true);

    let mut st = vec![0; m + 1];
    st[0] = 1;

    for i in 1..counts.size {
        let tmp = (1..=i)
            .filter(|&j| weights[j])
            .map(|j| st[i - j] % MOD)
            .sum::<i64>();
        counts[i] = tmp % MOD;

        for j in 0..=i {
            st[i] = (st[i] + counts[j] * counts[i - j]) % MOD;
        }

        print!("{} ", counts[i]);
    }
}
