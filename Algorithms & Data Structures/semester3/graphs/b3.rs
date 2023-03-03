use std::cmp::min;

fn link(g: &mut Vec<Vec<usize>>, q: usize, w: usize) {
    g[q].push(w);
    g[w].push(q);
}

fn dfs(
    g: &Vec<Vec<usize>>,
    u: &mut Vec<bool>,
    r: &mut Vec<bool>,
    tin: &mut Vec<usize>,
    up: &mut Vec<usize>,
    v: i64,
    p: i64,
    mut T: usize,
) {
    u[v as usize] = true;
    tin[v as usize] = T;
    up[v as usize] = T;
    T += 1;
    let mut c = 0;

    for to in g[v as usize].iter() {
        if *to as i64 == p {
            continue;
        }

        if u[*to] {
            up[v as usize] = min(up[v as usize], tin[*to]);
        } else {
            dfs(g, u, r, tin, up, *to as i64, v, T);

            c += 1;
            up[v as usize] = min(up[v as usize], up[*to]);

            if up[*to] >= tin[v as usize] && p != -1 {
                r[v as usize] = true;
            }
        }
    }
    if p == -1 && c > 1 {
        r[v as usize] = true;
    }
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let nm = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let n = nm[0];
    let m = nm[1];

    let mut v = n;

    let mut T = 0;
    let mut g = vec![vec![]; n + m];
    let mut u = vec![false; n + m];
    let mut r = vec![false; n + m];
    let mut tin: Vec<usize> = vec![0; n + m];
    let mut up: Vec<usize> = vec![1_000_000_000; n + m];

    for _ in 0..m {
        buffer.clear();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read line!");
        let uvw = buffer
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap())
            .collect::<Vec<_>>();

        let x = uvw[0];
        let y = uvw[1];
        let z = uvw[2];

        link(&mut g, x - 1, v);
        link(&mut g, y - 1, v);
        link(&mut g, z - 1, v);

        v += 1;
    }

    dfs(&mut g, &mut u, &mut r, &mut tin, &mut up, 0, -1, T);

    let mut a: Vec<usize> = vec![];
    for i in n..n + m {
        if r[i] {
            a.push(i - n);
        }
    }

    println!("{}", a.len());
    for el in a {
        print!("{} ", el + 1);
    }
}
