use std::cmp::Ordering;
use std::io::{stdout, Write};

fn compare(i: usize, j: usize) -> Ordering {
    println!("1 {} {}", i, j);
    stdout().flush().unwrap();

    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");
    if buffer.trim() == "YES" {
        Ordering::Greater
    } else {
        Ordering::Less
    }
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let n = buffer.trim().parse::<usize>().unwrap();

    let mut lamps: Vec<usize> = (1..=n).collect();

    lamps.sort_by(|a, b| compare(*a, *b));

    print!("0 ");

    for lamp in lamps.into_iter().rev() {
        print!("{} ", lamp);
    }

    println!();
}
