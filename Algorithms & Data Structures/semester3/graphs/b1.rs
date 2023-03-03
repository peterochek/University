const N: usize = 200001;
const E_18: i64 = 1_000_000_000_000_000_000;
const E_9: i64 = 1_000_000_000;

fn dfs(
    vertex: usize,
    graph: &Vec<Vec<(usize, i64)>>,
    topological: &mut Vec<usize>,
    used: &mut Vec<bool>,
) {
    used[vertex] = true;
    for (to, _) in graph[vertex].iter() {
        if !used[*to] {
            dfs(*to, graph, topological, used);
        }
    }

    topological.push(vertex);
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let nmst = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let n = nmst[0];
    let m = nmst[1];
    let s = nmst[2];
    let t = nmst[3];

    let mut graph = vec![Vec::new(); N];
    let mut answer = vec![0; N];
    let mut used = vec![false; N];
    let mut topological = vec![];

    for _ in 0..m {
        buffer.clear();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read line!");
        let uvw = buffer
            .split_whitespace()
            .map(|s| s.parse::<i64>().unwrap())
            .collect::<Vec<_>>();

        let u = uvw[0] as usize;
        let v = uvw[1] as usize;
        let w = uvw[2];

        graph[u - 1].push((v - 1, w));
    }

    for i in 0..n {
        if !used[i] {
            dfs(i, &graph, &mut topological, &mut used);
        }
    }

    topological.reverse();

    answer.fill(E_18);
    answer[s - 1] = 0;

    let start_idx = topological.iter().position(|&x| x == s - 1).unwrap();

    for i in start_idx..n {
        let v = topological[i];
        for (to, dist) in graph[v].iter() {
            if answer[*to] > answer[v] + dist {
                answer[*to] = answer[v] + dist;
            }
        }
    }

    if answer[t - 1] >= E_9 {
        println!("Unreachable");
    } else {
        println!("{}", answer[t - 1]);
    }
}
