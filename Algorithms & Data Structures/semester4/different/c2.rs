use std::io::{self, BufRead};

const MAXN: usize = 110;
const MAXM: usize = 110;
const INF: i64 = i64::MAX;

#[derive(Default, Clone, Copy)]
struct Edge {
    from: usize,
    to: usize,
    c: i64,
    f: i64,
    x: isize,
    y: isize,
    rev: bool,
    reversed: Option<usize>,
}

impl Edge {
    fn new(from: usize, to: usize, c: i64, f: i64) -> Self {
        Self {
            from,
            to,
            c,
            f,
            x: -1,
            y: -1,
            rev: true,
            reversed: None,
        }
    }

    fn new_with_coords(
        from: usize,
        to: usize,
        c: i64,
        f: i64,
        x: isize,
        y: isize,
        rev: bool,
    ) -> Self {
        Self {
            from,
            to,
            c,
            f,
            x,
            y,
            rev,
            reversed: None,
        }
    }
}

fn get_in_id(i: usize, j: usize, m: usize) -> usize {
    i * m + j
}

fn get_out_id(i: usize, j: usize, m: usize) -> usize {
    i * m + j + MAXN * MAXM
}

fn add_edge(v: usize, u: usize, c: i64, edges: &mut Vec<Vec<Edge>>) {
    edges[v].push(Edge::new(v, u, c, 0));
    edges[u].push(Edge::new(u, v, 0, 0));
    let v_len = edges[v].len();
    let u_len = edges[u].len();
    edges[u][u_len - 1].reversed = Some(v_len - 1);
    edges[v][v_len - 1].reversed = Some(u_len - 1);
}

fn add_dir_edge(
    v: usize,
    u: usize,
    c: i64,
    x: isize,
    y: isize,
    rev: bool,
    edges: &mut Vec<Vec<Edge>>,
) {
    edges[v].push(Edge::new_with_coords(v, u, c, 0, x, y, rev));
}

fn bfs(s: usize, t: usize, edges: &Vec<Vec<Edge>>, d: &mut Vec<i32>) -> bool {
    d.iter_mut().for_each(|val| *val = -1);
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(s);
    d[s] = 0;

    while let Some(v) = queue.pop_front() {
        for edge in &edges[v] {
            if d[edge.to] == -1 && edge.f < edge.c {
                d[edge.to] = d[v] + 1;
                queue.push_back(edge.to);
            }
        }
    }

    d[t] != -1
}

fn dfs(
    v: usize,
    min_c: i64,
    t: usize,
    edges: &mut Vec<Vec<Edge>>,
    d: &Vec<i32>,
    del: &mut Vec<usize>,
) -> i64 {
    if v == t {
        return min_c;
    }

    while del[v] < edges[v].len() {
        let mut edge = edges[v][del[v]];
        if edge.f < edge.c && d[edge.to] as i64 == d[v] as i64 + 1 {
            let pushed = dfs(edge.to, (min_c).min(edge.c - edge.f), t, edges, d, del);
            if pushed > 0 {
                edge.f += pushed;
                if edge.rev {
                    if let Some(rev_idx) = edge.reversed {
                        edges[edge.to][rev_idx].f -= pushed;
                    }
                }
                return pushed;
            }
        }
        del[v] += 1;
    }
    0
}

fn algo_dinic(s: usize, t: usize, edges: &mut Vec<Vec<Edge>>) -> i64 {
    let mut max_flow = 0;
    let mut d = vec![-1; MAXN * MAXM];
    let mut del = vec![0; MAXN * MAXM];

    while bfs(s, t, edges, &mut d) {
        del.iter_mut().for_each(|val| *val = 0);
        let mut pushed_flow = dfs(s, INF, t, edges, &d, &mut del);
        while pushed_flow != 0 {
            max_flow += pushed_flow;
            pushed_flow = dfs(s, INF, t, edges, &d, &mut del);
        }
    }

    max_flow
}

fn dfs2(v: usize, edges: &Vec<Vec<Edge>>, visited: &mut Vec<bool>, reached: &mut Vec<usize>) {
    visited[v] = true;
    reached.push(v);

    for edge in &edges[v] {
        if !visited[edge.to] && edge.f < edge.c {
            dfs2(edge.to, edges, visited, reached);
        }
    }
}

fn find_min_cut(
    s: usize,
    edges: &Vec<Vec<Edge>>,
    reached: &mut Vec<usize>,
    kingdom: &mut Vec<Vec<char>>,
) {
    let mut visited = vec![false; MAXN * MAXM];
    dfs2(s, edges, &mut visited, reached);

    for &mut v in reached {
        for edge in &edges[v] {
            if !visited[edge.to] && edge.f == 1 {
                let x = edge.x as usize;
                let y = edge.y as usize;
                kingdom[x][y] = '+';
                break;
            }
        }
    }
}

pub fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(|line| line.unwrap());
    let mut max_flow = 0;

    let mut kingdom = vec![vec![' '; MAXM]; MAXN];
    let nm: Vec<usize> = lines
        .next()
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect();
    let n = nm[0];
    let m = nm[1];

    let mut edges = vec![vec![]; MAXN * MAXM];
    let mut d = vec![-1; MAXN * MAXM];
    let mut del = vec![0; MAXN * MAXM];

    for i in 0..n {
        let line = lines.next().unwrap();
        for (j, ch) in line.chars().enumerate() {
            kingdom[i][j] = ch;
            match ch {
                '-' => add_dir_edge(
                    get_in_id(i, j, m),
                    get_out_id(i, j, m),
                    INF,
                    i as isize,
                    j as isize,
                    false,
                    &mut edges,
                ),
                '.' => add_dir_edge(
                    get_in_id(i, j, m),
                    get_out_id(i, j, m),
                    1,
                    i as isize,
                    j as isize,
                    false,
                    &mut edges,
                ),
                '#' => add_dir_edge(
                    get_in_id(i, j, m),
                    get_out_id(i, j, m),
                    0,
                    i as isize,
                    j as isize,
                    false,
                    &mut edges,
                ),
                'A' => add_dir_edge(
                    get_in_id(i, j, m),
                    get_out_id(i, j, m),
                    INF,
                    i as isize,
                    j as isize,
                    false,
                    &mut edges,
                ),
                'B' => add_dir_edge(
                    get_in_id(i, j, m),
                    get_out_id(i, j, m),
                    INF,
                    i as isize,
                    j as isize,
                    false,
                    &mut edges,
                ),
                _ => (),
            }
        }
    }

    for i in 0..n - 1 {
        for j in 0..m - 1 {
            add_edge(get_out_id(i, j, m), get_in_id(i + 1, j, m), INF, &mut edges);
            add_edge(get_out_id(i + 1, j, m), get_in_id(i, j, m), INF, &mut edges);
            add_edge(get_out_id(i, j, m), get_in_id(i, j + 1, m), INF, &mut edges);
            add_edge(get_out_id(i, j + 1, m), get_in_id(i, j, m), INF, &mut edges);
        }
    }

    for i in 0..n - 1 {
        add_edge(
            get_out_id(i, m - 1, m),
            get_in_id(i + 1, m - 1, m),
            INF,
            &mut edges,
        );
        add_edge(
            get_out_id(i + 1, m - 1, m),
            get_in_id(i, m - 1, m),
            INF,
            &mut edges,
        );
    }

    for j in 0..m - 1 {
        add_edge(
            get_out_id(n - 1, j, m),
            get_in_id(n - 1, j + 1, m),
            INF,
            &mut edges,
        );
        add_edge(
            get_out_id(n - 1, j + 1, m),
            get_in_id(n - 1, j, m),
            INF,
            &mut edges,
        );
    }

    max_flow = algo_dinic(
        get_out_id(n - 1, m - 1, m),
        get_in_id(n - 1, m - 1, m),
        &mut edges,
    );
    let mut reached = Vec::new();
    find_min_cut(
        get_out_id(n - 1, m - 1, m),
        &edges,
        &mut reached,
        &mut kingdom,
    );

    if max_flow >= INF {
        println!("-1");
    } else {
        println!("{}", max_flow);
        for row in kingdom {
            for ch in row {
                print!("{}", ch);
            }
            println!();
        }
    }
}
