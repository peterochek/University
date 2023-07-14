use std::io::{self, BufRead};

fn kuhn(u: usize, graph: &Vec<Vec<usize>>, matching: &mut Vec<i32>, used: &mut Vec<bool>) -> bool {
    if used[u] {
        return false;
    }
    used[u] = true;
    for &v in &graph[u] {
        if matching[v] == -1 || kuhn(matching[v] as usize, graph, matching, used) {
            matching[v] = u as i32;
            return true;
        }
    }
    false
}

pub fn main() {
    let stdin = io::stdin();
    let mut input = stdin.lock().lines();

    let mut line = input.next().unwrap().unwrap();
    let mut iter = line.split_whitespace();
    let n: usize = iter.next().unwrap().parse().unwrap();
    let m: usize = iter.next().unwrap().parse().unwrap();

    let mut graph: Vec<Vec<usize>> = vec![vec![]; n];
    let mut matching: Vec<i32> = vec![-1; m];
    let mut used: Vec<bool> = vec![false; n];

    for i in 0..n {
        line = input.next().unwrap().unwrap();
        iter = line.split_whitespace();
        let mut v: usize = iter.next().unwrap().parse().unwrap();
        while v != 0 {
            graph[i].push(v - 1);
            v = iter.next().unwrap().parse().unwrap();
        }
    }

    for i in 0..n {
        used = vec![false; n];
        kuhn(i, &graph, &mut matching, &mut used);
    }

    let mut count = 0;
    for &m in &matching {
        if m != -1 {
            count += 1;
        }
    }

    println!("{}", count);

    for (i, &m) in matching.iter().enumerate() {
        if m != -1 {
            println!("{} {}", m + 1, i + 1);
        }
    }
}
