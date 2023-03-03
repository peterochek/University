use std::cmp::min;

fn upd_z_array(z_array: &mut [usize], image: &Vec<usize>, i: usize, left: usize, right: usize) {
    if i <= right {
        z_array[i] = min(z_array[i - left], right - i + 1);
    }

    while i + z_array[i] < image.len() && image[i + z_array[i]] == image[z_array[i]] {
        z_array[i] += 1;
    }
}

pub fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed io input!");

    let nm = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let n = nm[0];
    let m = nm[1];

    buffer.clear();
    std::io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line!");

    let colors = buffer
        .split_whitespace()
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    let mut image = colors.clone();
    image.push(m + 1);
    image.extend(colors.iter().rev());

    let mut z_array = vec![0; image.len()];
    let mut present = false;
    let (mut left, mut right, mut marker) = (0, 0, n / 2);

    for i in 1..image.len() {
        upd_z_array(&mut z_array, &image, i, left, right);

        if i + marker * 2 == image.len() && present {
            if z_array[i] >= marker {
                print!("{} ", n - marker);
            }

            marker -= 1;
        }

        if image[i] == m + 1 {
            present = true;
        }

        if right < i + z_array[i] - 1 {
            right = i + z_array[i] - 1;
            left = i;
        }
    }

    println!("{}", n);
}
