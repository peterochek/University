use std::cmp::max;
use std::collections::BTreeMap;

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let mut s = buffer
        .trim()
        .as_bytes()
        .iter()
        .map(|x| *x as usize)
        .collect::<Vec<_>>();

    s.push(0);
    let n = s.len();

    let (mut counter, mut split) = (0, 0);
    let mut count = vec![0; n];
    let mut p_arr = vec![0; n];

    let mut t: BTreeMap<usize, Vec<usize>> = BTreeMap::new();

    for i in 0..n {
        t.entry(s[i]).or_default().push(i);
    }

    for (_, v) in t {
        for u in v {
            count[u] = split;
            p_arr[counter] = u;
            counter += 1;
        }

        split += 1;
    }

    let mut l = 1;
    while split < n {
        let mut a: Vec<Vec<usize>> = vec![vec![]; split];
        let mut inner_count = vec![0; n];
        let delim = (1 << l) / 2;
        let mut under_split = 0;
        counter = 0;

        for el in p_arr.iter() {
            let k = (*el + n - delim) % n;
            a[count[k]].push(k);
        }

        for i in 0..split {
            for j in 0..a[i].len() {
                if j == 0 || count[(a[i][j] + delim) % n] != count[(a[i][j - 1] + delim) % n] {
                    under_split += 1;
                }
                inner_count[a[i][j]] = under_split - 1;
                p_arr[counter] = a[i][j];
                counter += 1;
            }
        }

        count = inner_count;
        split = under_split;

        l += 1;
    }

    let mut present: i64 = 0;
    let mut ans = vec![0; n];
    for i in 0..n {
        if count[i] == n - 1 {
            continue;
        }
        let sled = p_arr[count[i] + 1];
        while max(i, sled) + (present as usize) < n
            && s[i + present as usize] == s[sled + present as usize]
        {
            present += 1;
        }
        ans[count[i]] = present;
        present = max(0, present - 1);
    }

    let mut unique = 0;
    for i in 1..n {
        unique += s.len() - ans[i - 1] as usize - p_arr[i] - 1;
    }
    println!("{unique}");
}
