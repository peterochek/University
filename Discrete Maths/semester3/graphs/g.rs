fn dfs(v: usize, matrix: &Vec<Vec<usize>>, colorings: &mut Vec<usize>, k: usize) {
    let neighbour_colors = matrix[v]
        .iter()
        .filter(|u| colorings[**u] != 0)
        .map(|u| colorings[*u])
        .collect::<Vec<usize>>();

    colorings[v] = (1..=k)
        .filter(|c| !neighbour_colors.contains(c))
        .min()
        .unwrap_or(0);

    matrix[v].iter().for_each(|u| {
        if colorings[*u] == 0 {
            dfs(*u, matrix, colorings, k);
        }
    });
}

pub fn main() {
    let mut buffer = String::new();

    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line!");

    let n_m = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let (n, m) = (n_m[0], n_m[1]);

    let mut matrix: Vec<Vec<usize>> = vec![vec![]; n + 1];

    for _ in 0..m {
        buffer.clear();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read line!");
        let pair = buffer
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap())
            .collect::<Vec<_>>();
        matrix[pair[0]].push(pair[1]);
        matrix[pair[1]].push(pair[0]);
    }

    let max_degree = matrix.iter().map(|s| s.len()).max().unwrap_or(0);

    let k = if max_degree % 2 == 0 {
        max_degree + 1
    } else {
        max_degree
    };

    println!("{}", k);

    let mut colorings = vec![0; n + 1];

    dfs(1, &matrix, &mut colorings, k);

    colorings[1..].iter().for_each(|c| println!("{}", c));
}
