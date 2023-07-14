use std::collections::VecDeque;
use std::fmt::Debug;

const MAX_VALUE: i64 = i64::MAX;

#[derive(Clone)]
struct E {
    u: usize,
    v: usize,
    f: i64,
    max: i64,
    c: i64,
    ord: i64,
}

impl E {
    fn new(u: usize, v: usize, f: i64, max: i64, c: i64) -> E {
        E {
            u,
            v,
            f,
            max,
            c,
            ord: -1,
        }
    }

    fn rev(&self) -> Self {
        E {
            u: self.v,
            v: self.u,
            f: -self.f,
            max: 0,
            c: -self.c,
            ord: -1,
        }
    }
}

impl Debug for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}, {}, {}, {}, {}, {})",
            self.u, self.v, self.f, self.max, self.c, self.ord
        )
    }
}

fn append(es: &mut Vec<Vec<E>>, u: usize, v: usize, max: i64, c: i64) {
    let e = E::new(u, v, 0, max, c);
    es[u].push(e.clone());
    es[v].push(e.rev());

    let u_size = es[u].len();
    let v_size = es[v].len();
    es[u][u_size - 1].ord = (v_size - 1) as i64;
    es[v][v_size - 1].ord = (u_size - 1) as i64;
}

fn compare(dist: &Vec<i64>, e: &E) -> bool {
    dist[e.v] > dist[e.u] + e.c && e.f < e.max
}

fn impl_fn(es: &mut Vec<Vec<E>>, n: usize) -> i64 {
    let mut ans: i64 = 0;

    let mut cnt = 0;

    loop {
        cnt += 1;

        let mut queue: VecDeque<usize> = VecDeque::new();
        let mut same: Vec<usize> = vec![0; n + 1];
        let mut dist: Vec<i64> = vec![MAX_VALUE; n + 1];
        let mut prev = Vec::with_capacity(n + 1);

        dist[0] = 0;
        queue.push_back(0);

        while !queue.is_empty() {
            let start = queue.pop_front().unwrap();
            same[start] = 2;

            for e in &es[start] {
                if compare(&dist, e) {
                    dist[e.v] = dist[e.u] + e.c;

                    if same[e.v] == 0 {
                        queue.push_back(e.v);
                    } else if same[e.v] == 2 {
                        queue.push_front(e.v);
                    }

                    same[e.v] = 1;
                    prev[e.v] = e;
                }
            }
        }

        let mut rem = MAX_VALUE;

        if dist[n - 1] == MAX_VALUE {
            break;
        } else {
            let mut vrtx = n - 1;
            while vrtx != 0 {
                let e = prev[vrtx];
                rem = rem.min(e.max - e.f);
                vrtx = prev[vrtx].u;
            }

            let mut vrtx = n - 1;
            while vrtx != 0 {
                let e = prev.get_mut(vrtx).unwrap();
                let rev = &mut es[e.v][e.ord as usize];

                e.f += rem;
                rev.f -= rem;

                ans += rem * e.c;
                vrtx = prev[vrtx].u;
            }
        }

        if cnt > 3 {
            break;
        }

        println!("cnt: {}", cnt);
        println!("prev: {:?}", prev);
        println!("same: {:?}", same);
        println!("dist: {:?}", dist);
        println!("es: {:?}", es);
    }

    ans
}

pub fn main() {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    let mut iter = input.split_whitespace();
    let n: usize = iter.next().unwrap().parse().unwrap();
    let m: usize = iter.next().unwrap().parse().unwrap();

    let mut es: Vec<Vec<E>> = vec![];

    for _ in 0..=2 * n + 1 {
        es.push(Vec::new());
    }

    let mut costs_input = String::new();
    std::io::stdin().read_line(&mut costs_input).unwrap();
    let mut costs_iter = costs_input.split_whitespace();
    let costs: Vec<i64> = costs_iter.map(|x| x.parse().unwrap()).collect();

    for i in 1..=n {
        append(&mut es, 0, n + i, 1, 0);
        append(&mut es, i, n + i, MAX_VALUE, 0);
        append(&mut es, i, 2 * n + 1, 1, 0);
        append(&mut es, n + i, i, MAX_VALUE, costs[i - 1]);
    }

    for _ in 0..m {
        let mut edge_input = String::new();
        std::io::stdin().read_line(&mut edge_input).unwrap();
        let mut edge_iter = edge_input.split_whitespace();
        let u: usize = edge_iter.next().unwrap().parse().unwrap();
        let v: usize = edge_iter.next().unwrap().parse().unwrap();
        let cost: i64 = edge_iter.next().unwrap().parse().unwrap();
        append(&mut es, n + u, v, MAX_VALUE, cost);
    }

    let n = 2 * (n + 1);
    let ans = impl_fn(&mut es, n);

    println!("{}", ans);
}
