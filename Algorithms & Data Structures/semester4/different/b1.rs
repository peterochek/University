struct Graph {
    boys_count: usize,
    girls_count: usize,
    pairs: Vec<Vec<i32>>,
    boys_matching: Vec<i32>,
    girls_matching: Vec<i32>,
    visited: Vec<bool>,
}

impl Graph {
    fn new(boys_count: usize, girls_count: usize) -> Self {
        Graph {
            boys_count,
            girls_count,
            pairs: vec![Vec::new(); boys_count + 1],
            boys_matching: vec![-1; boys_count + 1],
            girls_matching: vec![-1; girls_count + 1],
            visited: vec![false; boys_count + 1],
        }
    }

    fn dfs(&mut self, vertex: i32) {
        self.visited[vertex as usize] = true;
        let edges = self.pairs[vertex as usize].clone();
        for edge in edges {
            if self.girls_matching[edge as usize] != -1
                && !self.visited[self.girls_matching[edge as usize] as usize]
            {
                self.dfs(self.girls_matching[edge as usize]);
            }
        }
    }

    fn algo_impl(&mut self, vertex: i32) -> bool {
        self.visited[vertex as usize] = true;
        let edges = self.pairs[vertex as usize].clone();
        for edge in edges {
            if self.girls_matching[edge as usize] == -1
                || (!self.visited[self.girls_matching[edge as usize] as usize]
                    && self.algo_impl(self.girls_matching[edge as usize]))
            {
                self.girls_matching[edge as usize] = vertex;
                self.boys_matching[vertex as usize] = edge;

                return true;
            }
        }

        false
    }

    fn algo(&mut self) {
        for v in 1..=self.boys_count as i32 {
            self.visited = vec![false; self.boys_count + 1];
            if !self.visited[v as usize] {
                self.algo_impl(v);
            }
        }
    }

    fn invert_graph(&mut self, raw_graph: Vec<Vec<bool>>) {
        for v in 1..=self.boys_count as i32 {
            for u in 1..=self.girls_count as i32 {
                if !raw_graph[v as usize][u as usize] {
                    self.pairs[v as usize].push(u);
                }
            }
        }
    }

    fn matching(&mut self, raw_g: Vec<Vec<bool>>) {
        self.invert_graph(raw_g);
        self.algo();
    }

    fn cover(&mut self) -> (Vec<bool>, Vec<bool>) {
        self.visited = vec![false; self.boys_count + 1];
        for v in 1..=self.boys_count as i32 {
            if self.boys_matching[v as usize] == -1 {
                self.dfs(v);
            }
        }

        let mut left = vec![false; self.boys_count + 1];
        let mut right = vec![false; self.girls_count + 1];
        for v in 1..=self.boys_count as i32 {
            if !self.visited[v as usize] {
                left[v as usize] = true;
            } else if self.boys_matching[v as usize] != -1 {
                right[self.boys_matching[v as usize] as usize] = true;
            }
        }

        (left, right)
    }

    fn pairs(&self, boys: &[bool], girls: &[bool]) -> (Vec<i32>, Vec<i32>) {
        let mut left = Vec::new();
        let mut right = Vec::new();
        for v in 1..=self.boys_count as i32 {
            if !boys[v as usize] {
                left.push(v);
            }
        }
        for v in 1..=self.girls_count as i32 {
            if !girls[v as usize] {
                right.push(v);
            }
        }

        (left, right)
    }
}

pub fn main() {
    let mut lines = std::io::stdin().lines();

    let k: usize = lines.next().unwrap().unwrap().parse().unwrap();

    for _ in 0..k {
        let input: Vec<usize> = lines
            .next()
            .unwrap()
            .unwrap()
            .split_whitespace()
            .map(|x| x.parse().unwrap())
            .collect();

        let m = input[0];
        let n = input[1];

        let mut raw_g = vec![vec![false; n + 1]; m + 1];
        for v in 1..=m {
            let line = lines.next();
            let nums: Vec<usize> = line
                .unwrap()
                .unwrap()
                .split_whitespace()
                .map(|x| x.parse().unwrap())
                .collect();

            nums.iter()
                .take_while(|&x| *x != 0)
                .for_each(|&x| raw_g[v][x] = true);
        }

        let mut g = Graph::new(m, n);
        g.matching(raw_g);

        let (boys_c, girls_c) = g.cover();

        let (boys_pair, girls_pair) = g.pairs(&boys_c, &girls_c);

        println!("{}", boys_pair.len() + girls_pair.len());
        println!("{} {}", boys_pair.len(), girls_pair.len());

        for v in boys_pair {
            print!("{} ", v);
        }

        println!();

        for v in girls_pair {
            print!("{} ", v);
        }

        println!();
    }
}
