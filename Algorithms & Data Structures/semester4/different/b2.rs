use std::collections::VecDeque;

#[derive(Clone)]
struct E {
    a: usize,
    b: usize,
    cost: i32,
    flow: i32,
}

fn dfs(v: usize, g: &Vec<Vec<usize>>, e: &Vec<E>, visited: &mut Vec<bool>) {
    visited[v] = true;

    for &edge in &g[v] {
        if e[edge].flow < e[edge].cost && !visited[e[edge].b] {
            dfs(e[edge].b, g, e, visited);
        }
    }
}

fn dfs_flow(
    u: usize,
    f: i32,
    graph: &mut Vec<Vec<usize>>,
    e: &mut Vec<E>,
    dist: &mut Vec<i32>,
    p: &mut Vec<usize>,
    t: usize,
) -> i32 {
    if u == t || f == 0 {
        return f;
    }
    while p[u] < graph[u].len() {
        let id = graph[u][p[u]];
        let vertex = e[id].b;
        if dist[vertex] == dist[u] + 1 {
            let new = dfs_flow(
                vertex,
                f.min(e[id].cost - e[id].flow),
                graph,
                e,
                dist,
                p,
                t,
            );

            if new != 0 {
                e[id].flow += new;
                e[id ^ 1].flow -= new;
                return new;
            }
        }

        p[u] += 1;
    }

    0
}

fn path(
    n: usize,
    s: usize,
    t: usize,
    g: &Vec<Vec<usize>>,
    e: &Vec<E>,
) -> (Vec<i32>, bool) {
    let mut dist = vec![i32::MAX; n];
    let mut queue = VecDeque::new();
    dist[s] = 0;
    queue.push_back(s);

    while let Some(u) = queue.pop_front() {
        for &idx in &g[u] {
            let v = e[idx].b;
            if dist[v] == i32::MAX && e[idx].flow < e[idx].cost {
                dist[v] = dist[u] + 1;
                queue.push_back(v);
            }
        }
    }

    (dist.clone(), dist[t] != i32::MAX)
}

fn algo_impl(
    n: usize,
    s: usize,
    t: usize,
    graph: &mut Vec<Vec<usize>>,
    edges: &mut Vec<E>,
) -> i32 {
    let mut flow = 0;
    let mut p = vec![0; n];
    let mut d;
    let mut has_path = true;

    while has_path {
        let result = path(n, s, t, &graph, &edges);
        d = result.0;
        has_path = result.1;
        p = vec![0; n];
        let mut f = dfs_flow(s, i32::MAX, graph, edges, &mut d, &mut p, t);
        while f != 0 {
            flow += f;
            f = dfs_flow(s, i32::MAX, graph, edges, &mut d, &mut p, t);
        }
    }

    flow
}

fn input_string() -> String {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

pub fn main() {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let mut iter = input.split_whitespace();
    let n: usize = iter.next().unwrap().parse().unwrap();
    let m: usize = iter.next().unwrap().parse().unwrap();
    let mut g = vec![Vec::new(); n];
    let mut e = Vec::new();
    let mut visited = vec![false; n];
    let s = 0;
    let t = n - 1;

    for _ in 0..m {
        let input = input_string();
        let mut iter = input.split_whitespace();
        let a: usize = iter.next().unwrap().parse::<usize>().unwrap() - 1;
        let b: usize = iter.next().unwrap().parse::<usize>().unwrap() - 1;
        let c: i32 = iter.next().unwrap().parse().unwrap();

        g[a].push(e.len());
        e.push(E {
            a,
            b,
            cost: c,
            flow: 0,
        });
        g[b].push(e.len());
        e.push(E {
            a: b,
            b: a,
            cost: c,
            flow: 0,
        });
    }

    let min_f = algo_impl(n, s, t, &mut g, &mut e);
    let mut answer = Vec::new();

    dfs(s, &g, &e, &mut visited);
    for i in (0..2 * m).step_by(2) {
        if (visited[e[i].a] && !visited[e[i].b])
            || (!visited[e[i].a] && visited[e[i].b])
        {
            answer.push(i / 2);
        }
    }

    println!("{} {}", answer.len(), min_f);

    for i in answer {
        print!("{} ", i + 1);
    }
}