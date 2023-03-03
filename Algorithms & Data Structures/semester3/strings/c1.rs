use std::io;

fn pref(s: &str) -> Vec<usize> {
    let mut j;

    let mut p = vec![0; s.len()];
    p[0] = 0;
    for i in 1..s.len() {
        j = p[i - 1];

        while (j > 0) && (s.as_bytes()[i] != s.as_bytes()[j]) {
            j = p[j - 1];
        }

        if s.as_bytes()[i] == s.as_bytes()[j] {
            p[i] = j + 1;
        } else {
            p[i] = 0;
        }
    }

    p
}

pub fn main() {
    let mut s = String::new();
    io::stdin()
        .read_line(&mut s)
        .expect("Failed to read from stdin");

    let s = s.trim();

    let len = s.len();

    let p = pref(&s);

    let mut period = len - p[len - 1];

    if len % period != 0 {
        period = len;
    }

    println!("{}", period);
}
