use std::collections::LinkedList;

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let n = buffer.trim().parse::<usize>().unwrap();

    let mut matrix = vec![vec![false; n]; n];

    for i in 0..n {
        buffer.clear();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read line!");
        buffer
            .trim()
            .chars()
            .map(|s| s.to_digit(10).unwrap())
            .enumerate()
            .for_each(|(j, c)| {
                if c == 1 {
                    matrix[i][j] = true;
                } else {
                    matrix[j][i] = true;
                }
            });
    }

    let mut p = vec![0];

    for i in 1..n {
        let mut idx = 0;
        while idx != p.len() && matrix[idx][i] {
            idx += 1;
        }
        p.insert(idx, i);
    }

    let mut idx = p.len() - 1;
    while matrix[p[0]][idx] {
        idx -= 1;
    }

    if idx == p.len() - 1 {
        p.iter().rev().for_each(|i| print!("{} ", i + 1));
        return;
    }

    idx += 1;

    let mut q = LinkedList::new();
    for val in p[idx..].iter() {
        q.push_back(*val);
    }

    p.drain(idx..);

    while !q.is_empty() {
        let mut inner_idx = 0;
        let u = *q.front().unwrap();
        q.pop_front();
        while inner_idx != p.len() && matrix[inner_idx][u] {
            inner_idx += 1;
        }
        if inner_idx != p.len() {
            p.insert(inner_idx, u);
        } else {
            q.push_back(u);
        }
    }

    p.iter().rev().for_each(|i| print!("{} ", i + 1));
}
