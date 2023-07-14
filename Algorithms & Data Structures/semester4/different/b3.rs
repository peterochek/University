use std::fs::File;
use std::io::{BufReader, BufRead, Write};
use std::fs;

fn hung(matrix: Vec<Vec<i32>>, n: usize, m: usize) -> (i32, Vec<i32>) {
    let mut a: Vec<i32> = vec![0; n + 1];
    let mut b: Vec<i32> = vec![0; m + 1];
    let mut matching: Vec<i32> = vec![0; m + 1];
    let mut path: Vec<i32> = vec![0; m + 1];

    for i in 1..=m {
        matching[0] = i as i32;
        let mut col = 0;
        let mut cur_min: Vec<i32> = vec![i32::MAX; m + 1];
        let mut visited: Vec<bool> = vec![false; m + 1];

        loop {
            visited[col] = true;
            let row = matching[col] as usize;
            let mut rem = i32::MAX;
            let mut min_cur = 0;

            for j in 1..=m {
                if !visited[j] {
                    let current = matrix[row][j] - a[row] - b[j];

                    if current < cur_min[j] {
                        cur_min[j] = current;
                        path[j] = col as i32;
                    }

                    if cur_min[j] < rem {
                        rem = cur_min[j];
                        min_cur = j;
                    }
                }
            }

            for j in 0..=m {
                if visited[j] {
                    a[matching[j] as usize] += rem;
                    b[j] -= rem;
                } else {
                    cur_min[j] -= rem;
                }
            }

            col = min_cur;
            if matching[col] == 0 {
                break;
            }
        }

        loop {
            let min_column = path[col] as usize;
            matching[col] = matching[min_column];
            col = min_column;
            if col == 0 {
                break;
            }
        }
    }

    let mut answer: Vec<i32> = vec![0; n + 1];
    for j in 1..=m {
        answer[matching[j] as usize] = j as i32;
    }

    (-b[0], answer)
}

pub fn main() {
    let mut buf = String::new();

    let mut reader = BufReader::new(fs::File::open("assignment.in").unwrap());

    reader.read_line(&mut buf).unwrap();

    let n: usize = buf.trim().parse().unwrap();

    let mut matrix: Vec<Vec<i32>> = vec![vec![0; n + 1]; n + 1];

    for i in 1..=n {
        buf.clear();
        reader.read_line(&mut buf).unwrap();
        let row: Vec<i32> = buf
            .trim()
            .split_whitespace()
            .map(|x| x.parse().unwrap())
            .collect();

        for j in 1..=n {
            matrix[i][j] = row[j - 1];
        }
    }

    println!("{:?}", matrix);

    let (cost, assignment) = hung(matrix, n, n);

    let mut file = File::create("assignment.out").unwrap();
    let mut repr = cost.to_string();
    repr += "\n";
    file.write(repr.as_bytes()).unwrap();

    for i in 1..=n {
        repr.clear();
        repr = i.to_string() + " " + assignment[i].to_string().as_str();
        repr += "\n";
        file.write(repr.as_bytes()).unwrap();
    }
}
