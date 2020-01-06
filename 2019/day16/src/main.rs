use itertools;

fn get_digits(s: &str) -> Vec<isize> {
    s.trim()
        .chars()
        .map(|x| x.to_digit(10).unwrap() as isize)
        .collect()
}

fn get_pattern(l: usize, r: usize) -> Vec<isize> {
    let zeroes = itertools::repeat_n(0, r).collect::<Vec<_>>();
    let ones = itertools::repeat_n(1, r).collect::<Vec<_>>();
    let m_ones = itertools::repeat_n(-1, r).collect::<Vec<_>>();

    let init = zeroes
        .iter()
        .cloned()
        .take(r - 1)
        .chain(ones.iter().cloned())
        .chain(zeroes.iter().cloned())
        .chain(m_ones.iter().cloned())
        .collect::<Vec<_>>();
    let tail = zeroes
        .iter()
        .cloned()
        .chain(ones.iter().cloned())
        .chain(zeroes.iter().cloned())
        .chain(m_ones.iter().cloned())
        .collect::<Vec<_>>();
    //    .iter()
    init.iter()
        .chain(tail.iter().cycle())
        .take(l)
        .cloned()
        .collect()
}

fn get_offset(list: &Vec<isize>) -> isize {
    list.iter()
        .take(7)
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("")
        .parse::<isize>()
        .unwrap()
}

fn compute_phase_part1(input: Vec<isize>) -> Vec<isize> {
    let mut output = Vec::new();
    let l = input.len();
    for i in 1..(l + 1) {
        let new_digit = input
            .iter()
            .zip(get_pattern(l, i))
            .map(|(x, p)| x * p)
            .sum::<isize>();
        if new_digit < 0 {
            output.push((-1 * new_digit) % 10);
        } else {
            output.push(new_digit % 10);
        }
    }
    output
}

fn compute_phase_part2(input: Vec<isize>) -> Vec<isize> {
    let mut output: Vec<isize> = Vec::new();
    let l = input.len();
    let mut part_sum = 0;
    for i in (0..l).rev() {
        part_sum += input[i];
        output.push(part_sum % 10);
    }
    output.iter().cloned().rev().collect()
}

fn main() {
    let mut digits = get_digits(&include_str!("../input").trim());
    for _ in 0..100 {
        digits = compute_phase_part1(digits);
    }
    println!(
        "{}",
        digits
            .iter()
            .take(8)
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("")
    );

    let file = itertools::repeat_n(include_str!("../input"), 10000)
        .map(|x| x.trim())
        .collect::<Vec<_>>()
        .join("");
    let mut digits = get_digits(&file);
    let offset = get_offset(&digits);
    digits = digits.iter().cloned().skip(offset as usize).collect();
    for _ in 0..100 {
        digits = compute_phase_part2(digits);
    }
    println!(
        "{}",
        digits
            .iter()
            .take(8)
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("")
    );
}
