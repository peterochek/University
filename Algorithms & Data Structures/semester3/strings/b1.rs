fn prefix_fn(string: &str) -> Vec<usize> {
    let n = string.len();
    let mut prefix = vec![0; n];

    for i in 1..n {
        let mut j = prefix[i - 1];
        while j > 0 && string.as_bytes()[i] != string.as_bytes()[j] {
            j = prefix[j - 1];
        }
        if string.as_bytes()[i] == string.as_bytes()[j] {
            j += 1;
        };

        prefix[i] = j;
    }

    prefix
}

fn kmp(p: &str, t: &str) -> Vec<usize> {
    let new_str = format!("{}#{}", p, t);
    let prefix = prefix_fn(&new_str);
    let mut res = vec![];

    for i in p.len()..p.len() + 1 + t.len() {
        if prefix[i] == p.len() {
            res.push(i - p.len() * 2 + 1);
        }
    }

    res
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");
    let p = buffer.clone();
    buffer.clear();

    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");
    let t = buffer;

    let answer = kmp(p.trim(), t.trim());

    println!("{}", answer.len());
    for i in answer {
        print!("{} ", i);
    }
}
