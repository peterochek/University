use std::collections::VecDeque;

fn reverse_sub_queue(q: &mut VecDeque<usize>, i: usize) {
    let mut j = 0;
    while 1 + j < i - j {
        q.swap(1 + j, i - j);
        j += 1;
    }
}

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
                    matrix[j][i] = true;
                }
            });
    }

    let mut q = VecDeque::new();

    for v in 0..n {
        q.push_back(v);
    }

    let mut i = 0;

    while i < n * (n - 1) {
        if !matrix[q[0]][q[1]] {
            i = 2;
            while !matrix[q[0]][q[i]] || !matrix[q[1]][q[i + 1]] {
                i += 1;
            }

            reverse_sub_queue(&mut q, i);
        }

        q.push_back(*q.front().unwrap());
        q.pop_front();

        i += 1;
    }

    for v in q {
        print!("{} ", v + 1);
    }
}
