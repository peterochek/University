use std::io;

const MOD: i64 = 104_857_601;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let inputs = input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<usize>>();
    let k = inputs[0];
    let n = inputs[1];

    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let mut a = input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect::<Vec<i64>>();
    a.append(&mut vec![0; k]);

    input.clear();
    io::stdin().read_line(&mut input).unwrap();
    let mut c = input
        .split_whitespace()
        .map(|s| (MOD - s.parse::<i64>().unwrap()) % MOD)
        .collect::<Vec<i64>>();
    c.insert(0, 1);

    let mut ans = vec![0; k + 1];
    let mut new_c: Vec<i64>;

    let mut n = n - 1;

    while k <= n {
        for i in k..a.len() {
            a[i] = 0;
            for j in 1..c.len() {
                a[i] = (a[i] - c[j] * a[i - j]) % MOD;
                if a[i] < 0 {
                    a[i] += MOD;
                }
            }
        }

        new_c = c
            .iter()
            .enumerate()
            .map(|(i, &el)| if i % 2 == 0 { el } else { (MOD - el) % MOD })
            .collect();

        for i in (0..=2 * k).step_by(2) {
            let mut coef = 0;
            for j in 0..=i {
                let tmp = if j > k { 0 } else { c[j] };
                let trunk = if i - j > k { 0 } else { new_c[i - j] };
                coef = (coef + tmp * trunk + MOD) % MOD;
            }

            ans[i / 2] = coef;
        }

        c.copy_from_slice(&ans);

        let mut nxt = 0;
        for i in 0..a.len() {
            if i % 2 == n % 2 {
                a[nxt] = a[i];
                nxt += 1;
            }
        }

        n /= 2;
    }

    println!("{}", a[n]);
}
