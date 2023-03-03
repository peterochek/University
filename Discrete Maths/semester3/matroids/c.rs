use std::cmp::Reverse;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn dfs(
    g: &Vec<Vec<usize>>,
    matching: &mut Vec<usize>,
    prev: &mut Vec<usize>,
    used: &mut Vec<bool>,
    v: usize,
) -> bool {
    if used[v] {
        return false;
    }

    used[v] = true;
    for to in g[v].iter() {
        if matching[*to] == std::usize::MAX || dfs(g, matching, prev, used, matching[*to]) {
            prev[v] = *to;
            matching[*to] = v;
            return true;
        }
    }

    false
}

pub fn main() {
    let file = File::open("matching.in").expect("Failed to open file!");
    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    reader.read_line(&mut buffer).expect("Failed io input!");

    let n = buffer.trim().parse::<usize>().unwrap();

    buffer.clear();

    reader.read_line(&mut buffer).expect("Failed io input!");

    let mut pairs = buffer
        .split_whitespace()
        .enumerate()
        .map(|(idx, s)| (idx, s.parse::<usize>().unwrap()))
        .collect::<Vec<(usize, usize)>>();

    buffer.clear();

    pairs.sort_by_key(|a| Reverse(a.1));

    let mut g = vec![vec![]; n];

    for v in g.iter_mut() {
        reader.read_line(&mut buffer).expect("Failed io input!");
        buffer
            .split_whitespace()
            .skip(1)
            .map(|s| s.parse::<usize>().unwrap())
            .for_each(|u| v.push(u - 1));

        buffer.clear();
    }

    let mut matching = vec![std::usize::MAX; n];
    let mut prev = vec![std::usize::MAX; n];

    let mut used = vec![false; n];

    for v in pairs {
        for el in used.iter_mut() {
            *el = false;
        }
        dfs(&g, &mut matching, &mut prev, &mut used, v.0);
    }

    let mut str_repr = String::new();

    for right in prev {
        let new_num = match right {
            std::usize::MAX => 0,
            _ => right + 1,
        };

        str_repr.push_str(&new_num.to_string());
        str_repr.push(' ');
    }

    fs::write("matching.out", str_repr).expect("Failed to write to file!");
}
