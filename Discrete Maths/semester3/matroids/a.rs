use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn answer(mut todos: Vec<(u64, u64)>) -> u64 {
    todos.sort();

    let mut total = todos
        .iter()
        .filter(|task| task.0 == 0)
        .map(|task| task.1)
        .sum::<u64>();

    let mut greedy: BinaryHeap<Reverse<u64>> = BinaryHeap::new();

    let mut counter = 0;

    for todo in todos.iter().filter(|task| task.0 != 0) {
        if counter < todo.0 {
            counter += 1;
        } else {
            total += greedy.pop().unwrap().0;
        }

        greedy.push(Reverse(todo.1));
    }

    total
}

pub fn main() {
    let file = File::open("schedule.in").expect("Failed to open file!");
    let reader = BufReader::new(file);

    let mut todos = Vec::new();

    for line in reader.lines().skip(1) {
        let data = line
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<u64>>();

        todos.push((data[0], data[1]));
    }

    let answer = answer(todos);

    fs::write("schedule.out", answer.to_string()).expect("Failed to write to file!");
}

#[cfg(test)]
mod tests {
    #[test]
    fn check() {
        let tests: Vec<Vec<(u64, u64)>> = vec![
            vec![(1, 1), (2, 2), (3, 3)],
            vec![(2, 2), (3, 3), (0, 5)],
            vec![(0, 5), (3, 2), (3, 3)],
            vec![(3, 2), (2, 50), (3, 3)],
            vec![(3, 50), (3, 2), (3, 3)],
            vec![(1, 2), (2, 2)],
            vec![(1, 2), (1, 3)],
            vec![(2, 2), (2, 300), (2, 3), (2, 30)],
            vec![(0, 5), (1, 2), (1, 4), (2, 6), (4, 6), (4, 6), (4, 7)],
            vec![(1, 1), (1, 2)],
            vec![
                (0, 5),
                (1, 2),
                (1, 4),
                (2, 6),
                (4, 6),
                (4, 6),
                (4, 7),
                (4, 8),
            ],
            vec![
                (0, 5),
                (1, 2),
                (1, 4),
                (2, 6),
                (4, 6),
                (4, 6),
                (4, 7),
                (4, 6),
            ],
            vec![(1, 1), (1, 2), (2, 20), (2, 30)],
            vec![(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)],
        ];

        let answers = vec![0, 5, 5, 0, 0, 0, 2, 5, 11, 1, 17, 17, 3, 10];

        for (idx, (test, answer)) in tests.into_iter().zip(answers.into_iter()).enumerate() {
            assert_eq!(super::answer(test), answer, "idx {}", idx);
        }
    }
}
