#[derive(Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

fn distance(v: usize, to: usize, points: &[Point]) -> f32 {
    if v == to {
        return f32::MAX;
    }
    let a = points[v];
    let b = points[to];
    f32::sqrt(((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)) as f32)
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let n = buffer.trim().parse::<usize>().unwrap();

    let mut points = Vec::with_capacity(n);
    let mut used = vec![false; n];
    let mut selected = vec![usize::MAX; n];
    let mut min = vec![f32::MAX; n];

    min[0] = 0_f32;

    for _ in 0..n {
        buffer.clear();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("Failed to read line!");
        let pair = buffer
            .split_whitespace()
            .map(|s| s.parse::<i32>().unwrap())
            .collect::<Vec<_>>();

        points.push(Point {
            x: pair[0],
            y: pair[1],
        });
    }

    let mut result = 0_f32;
    for _ in 0..n {
        let mut v = usize::MAX;
        for j in 0..n {
            if !used[j] && (v == usize::MAX || min[j] < min[v]) {
                v = j;
            }
        }

        used[v] = true;
        if selected[v] != usize::MAX {
            result += distance(v, selected[v], &points);
        }

        for to in 0..n {
            let dist = distance(v, to, &points);
            if dist < min[to] {
                min[to] = dist;
                selected[to] = v;
            }
        }
    }

    println!("{}", result);
}
