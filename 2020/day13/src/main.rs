use day13::aoclib::*;

fn main() {
    let (min_time, ids) = parse_input("input");
    println!("Part 1: {}", solve1(min_time, &ids));
    println!("Part 2: {}", solve2(congruences(&ids)));
}

fn solve2(congruences: Vec<(i128, i128)>) -> i128 {
    let mut current = (0, congruences[0].1);
    for next in congruences[1..].iter() {
        let (a1, n1) = current;
        let (a2, n2) = next;

        let (m1, m2) = bezout(n1, *n2);

        let sol = a1 * m2 * n2 + a2 * m1 * n1;

        let new_mod = n1 * *n2;

        let new_val;
        if sol < 0 {
            new_val = ((sol % new_mod) + new_mod) % new_mod; // '%' in rust is remainder not modulo
        } else {
            new_val = sol % new_mod;
        }

        current = (new_val, new_mod);
    }
    current.0
}

fn congruences(ids: &Vec<Option<i64>>) -> Vec<(i128, i128)> {
    let mut result = Vec::new();
    for i in 0..ids.len() {
        match ids[i] {
            None => {}
            Some(n) => result.push(((n - i as i64) as i128, n as i128)),
        }
    }
    result
}

fn bezout(a: i128, b: i128) -> (i128, i128) {
    let (mut old_r, mut r) = (a, b);
    let (mut old_s, mut s) = (1, 0);
    let (mut old_t, mut t) = (0, 1);

    while r != 0 {
        let quotient = old_r / r;
        let (tmp1, tmp2) = (r, old_r - quotient * r);
        old_r = tmp1;
        r = tmp2;

        let (tmp1, tmp2) = (s, old_s - quotient * s);
        old_s = tmp1;
        s = tmp2;

        let (tmp1, tmp2) = (t, old_t - quotient * t);
        old_t = tmp1;
        t = tmp2;
    }

    (old_s, old_t)
}

fn parse_input(fname: &str) -> (i64, Vec<Option<i64>>) {
    let lines = read_file(fname).unwrap();
    let lines: Vec<_> = lines.lines().collect();
    let min_time = lines[0].parse::<i64>().unwrap();
    let bus_ids = lines[1].split(',').map(|x| x.parse::<i64>().ok()).collect();
    (min_time, bus_ids)
}

fn solve1(min_time: i64, bus_ids: &Vec<Option<i64>>) -> i64 {
    let (departure, id) = bus_ids
        .iter()
        .filter(|x| x.is_some())
        .map(|x| {
            (
                (min_time as f64 / x.unwrap() as f64).ceil() as i64 * x.unwrap(),
                x.unwrap(),
            )
        })
        .min_by(|x, y| x.0.cmp(&y.0))
        .unwrap();

    (departure - min_time) * id
}

#[test]
fn test_solve1() {
    let (mtime, ids) = parse_input("test");
    assert_eq!(solve1(mtime, &ids), 295);
}

#[test]
fn test_solve2() {
    let (mtime, ids) = parse_input("test");
    assert_eq!(solve2(congruences(&ids)), 1068781);
}

#[test]
fn test_solve2_1() {
    let (_, ids) = parse_input("test2");
    assert_eq!(solve2(congruences(&ids)), 3417);
}
