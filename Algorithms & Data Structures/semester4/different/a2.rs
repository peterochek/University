use std::collections::VecDeque;

struct E {
    b: usize,
    cost: i32,
    flow: i32,
}

type Edges = Vec<E>;
type Graph = Vec<Vec<usize>>;

fn dfs(
    vertex: usize,
    flow: i32,
    graph: &mut Graph,
    edges: &mut Edges,
    dist: &mut Vec<i32>,
    p: &mut Vec<usize>,
    t: usize,
) -> i32 {
    if vertex == t || flow == 0 {
        return flow;
    }

    while p[vertex] < graph[vertex].len() {
        let id = graph[vertex][p[vertex]];
        let v = edges[id].b;
        if dist[v] == dist[vertex] + 1 {
            let pushed = dfs(
                v,
                flow.min(edges[id].cost - edges[id].flow),
                graph,
                edges,
                dist,
                p,
                t,
            );

            if pushed != 0 {
                edges[id].flow += pushed;
                edges[id ^ 1].flow -= pushed;
                return pushed;
            }
        }

        p[vertex] += 1;
    }

    0
}

fn path(n: usize, s: usize, t: usize, graph: &Graph, edges: &Edges) -> (Vec<i32>, bool) {
    let mut dist = vec![i32::MAX; n];
    let mut queue = VecDeque::new();
    dist[s] = 0;
    queue.push_back(s);
    while let Some(u) = queue.pop_front() {
        for &id in &graph[u] {
            let v = edges[id].b;
            if dist[v] == i32::MAX && edges[id].flow < edges[id].cost {
                dist[v] = dist[u] + 1;
                queue.push_back(v);
            }
        }
    }

    (dist.clone(), dist[t] != i32::MAX)
}

fn input_string() -> String {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

pub fn main() {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let n: usize = input.trim().parse().unwrap();
    input.clear();
    std::io::stdin().read_line(&mut input).unwrap();
    let m: usize = input.trim().parse().unwrap();
    input.clear();
    let mut graph = vec![Vec::new(); n];
    let mut edges = Vec::new();
    let s = 0;
    let t = n - 1;

    for _ in 0..m {
        let input = input_string();
        let mut iter = input.split_whitespace();
        let a: usize = iter.next().unwrap().parse::<usize>().unwrap() - 1;
        let b: usize = iter.next().unwrap().parse::<usize>().unwrap() - 1;
        let c: i32 = iter.next().unwrap().parse().unwrap();

        graph[a].push(edges.len());
        edges.push(E {
            b,
            cost: c,
            flow: 0,
        });
        graph[b].push(edges.len());
        edges.push(E {
            b: a,
            cost: c,
            flow: 0,
        });
    }

    let mut flow = 0;
    let mut p;
    let mut d;
    let mut has_path = true;

    while has_path {
        let result = path(n, s, t, &graph, &edges);
        d = result.0;
        has_path = result.1;
        p = vec![0; n];
        let mut f = dfs(s, i32::MAX, &mut graph, &mut edges, &mut d, &mut p, t);
        while f != 0 {
            flow += f;
            f = dfs(s, i32::MAX, &mut graph, &mut edges, &mut d, &mut p, t);
        }
    }

    println!("{}", flow);

    for i in (0..edges.len()).step_by(2) {
        println!("{}", edges[i].flow);
    }
}
