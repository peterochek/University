pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let n = buffer.trim().parse::<usize>().unwrap();

    buffer.clear();

    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line!");

    let mut code = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    for v in code.iter_mut() {
        *v -= 1;
    }

    let mut degree = vec![1; n];
    for i in code.iter() {
        degree[*i] += 1;
    }

    let mut ptr = 0;
    while ptr < n && degree[ptr] != 1 {
        ptr += 1;
    }

    let mut leaf = ptr;

    let mut edges: Vec<(usize, usize)> = vec![];

    for v in code.into_iter() {
        edges.push((leaf, v));
        degree[v] -= 1;
        degree[leaf] -= 1;
        if degree[v] == 1 && v < ptr {
            leaf = v;
        } else {
            ptr += 1;
            while ptr < n && degree[ptr] != 1 {
                ptr += 1;
            }
            leaf = ptr;
        }
    }

    for v in 0..n - 1 {
        if degree[v] == 1 {
            edges.push((v, n - 1));
        }
    }

    for edge in edges {
        println!("{} {}", edge.0 + 1, edge.1 + 1);
    }
}
