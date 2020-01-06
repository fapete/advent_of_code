use std::fs;

fn new_deck(size: u32) -> Vec<u32> {
    (0..size).collect()
}

fn new_stack(mut deck: Vec<u32>) -> Vec<u32> {
    deck.reverse();
    deck
}

fn cut(mut deck: Vec<u32>, pos: i32) -> Vec<u32> {
    if pos < 0 {
        let i = deck.len();
        cut(deck, i as i32 - pos.abs())
    } else {
        let mut res = Vec::with_capacity(deck.len());
        let (left, right) = deck.split_at_mut(pos as usize);
        res.extend_from_slice(right);
        res.extend_from_slice(left);
        res
    }
}

fn increment(deck: Vec<u32>, i: u32) -> Vec<u32> {
    let mut res = Vec::with_capacity(deck.len());
    res.resize(deck.len(), 0);
    let mut idx = 0;
    for n in deck {
        res[idx] = n;
        idx = (idx + i as usize) % res.len();
    }
    res
}

#[derive(Debug, Eq, PartialEq)]
enum Shuffle {
    NewStack,
    Cut(i32),
    Increment(u32),
}

fn parse_shuffle_file(file: &str) -> Vec<Shuffle> {
    let mut res = Vec::new();
    let content = fs::read_to_string(file).unwrap();
    for line in content.lines() {
        if line.starts_with("deal into new stack") {
            res.push(Shuffle::NewStack);
        }
        if line.starts_with("cut") {
            let amount = line
                .split(" ")
                .collect::<Vec<_>>()
                .last()
                .unwrap()
                .parse::<i32>()
                .unwrap();
            res.push(Shuffle::Cut(amount));
        }
        if line.starts_with("deal with increment") {
            let amount = line
                .split(" ")
                .collect::<Vec<_>>()
                .last()
                .unwrap()
                .parse::<u32>()
                .unwrap();
            res.push(Shuffle::Increment(amount));
        }
    }
    res
}

fn part1(file: &str, deck_size: u32) -> Vec<u32> {
    let mut deck = new_deck(deck_size);
    let shuffling_strategy = parse_shuffle_file(file);
    for shuffle in shuffling_strategy {
        match shuffle {
            Shuffle::NewStack => deck = new_stack(deck),
            Shuffle::Cut(i) => deck = cut(deck, i),
            Shuffle::Increment(i) => deck = increment(deck, i),
        }
    }
    deck
}

fn inverse_new_stack(size: usize, idx: usize) -> usize {
    size - 1 - idx_in
}

fn inverse_cut(size: usize, idx: usize, cut: usize) -> usize {
    0
}

fn track_new_stack(size: usize, idx_in: usize) -> usize {
    size - idx_in - 1
}

fn track_cut(size: usize, idx_in: usize, cut_idx: isize) -> usize {
    if cut_idx < 0 {
        track_cut(size, idx_in, size as isize - cut_idx.abs())
    } else {
        if idx_in >= cut_idx as usize {
            idx_in - cut_idx as usize
        } else {
            size - cut_idx as usize + idx_in
        }
    }
}

fn track_increment(size: usize, idx_in: usize, inc: usize) -> usize {
    (idx_in * inc) % size
}

fn part2(file: &str, deck_size: usize, mut track_idx: usize, repetitions: u64) -> usize {
    let shuffling_strategy = parse_shuffle_file(file);
    for _ in 0..repetitions {
        for shuffle in &shuffling_strategy {
            track_idx = match shuffle {
                Shuffle::NewStack => track_new_stack(deck_size, track_idx),
                Shuffle::Cut(i) => track_cut(deck_size, track_idx, *i as isize),
                Shuffle::Increment(i) => track_increment(deck_size, track_idx, *i as usize),
            }
        }
    }
    track_idx
}

fn main() {
    let deck = part1("input", 10007);
    let idx_2019 = deck
        .iter()
        .enumerate()
        .find(|(_, x)| **x == 2019)
        .unwrap()
        .0;
    println!("Card 2019 is at: {}", idx_2019);

    // Part 2
    println!(
        "Card 2020 is at {} after 2 repetitions",
        part2("input", 10007, 2019, 2)
    );
    println!(
        "Card 2020 is at {} after 3 repetitions",
        part2("input", 10007, 2019, 3)
    );
    println!(
        "Card 2020 is at {} after 4 repetitions",
        part2("input", 10007, 2019, 4)
    );
    println!(
        "Card 2020 is at {} after 5 repetitions",
        part2("input", 10007, 2019, 5)
    );
    println!(
        "Card 2020 is at {} after many repetitions",
        part2("input", 119315717514047, 2020, 101741582076661)
    );
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_solve_part1() {
        assert_eq!(part1("ex1", 10), vec![0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
        assert_eq!(part1("ex2", 10), vec![3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
        assert_eq!(part1("ex3", 10), vec![6, 3, 0, 7, 4, 1, 8, 5, 2, 9]);
        assert_eq!(part1("ex4", 10), vec![9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
    }

    #[test]
    fn test_new_stack() {
        assert_eq!(
            new_stack(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0]),
            vec![0, 9, 8, 7, 6, 5, 4, 3, 2, 1]
        );
    }

    #[test]
    fn test_cut_pos() {
        assert_eq!(
            cut(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0], 4),
            vec![5, 6, 7, 8, 9, 0, 1, 2, 3, 4]
        );
    }

    #[test]
    fn test_cut_neg() {
        assert_eq!(
            cut(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0], -4),
            vec![7, 8, 9, 0, 1, 2, 3, 4, 5, 6]
        );
    }

    #[test]
    fn test_increment() {
        assert_eq!(
            increment(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0], 3),
            vec![1, 8, 5, 2, 9, 6, 3, 0, 7, 4]
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse_shuffle_file("ex2"),
            vec![Shuffle::Cut(6), Shuffle::Increment(7), Shuffle::NewStack]
        );
    }

    #[test]
    fn test_track_new_stack() {
        assert_eq!(track_new_stack(10, 6), 3);
        assert_eq!(track_new_stack(10, 9), 0);
        assert_eq!(track_new_stack(10, 0), 9);
    }

    #[test]
    fn test_track_cut_pos() {
        assert_eq!(track_cut(10, 6, 3), 3);
        assert_eq!(track_cut(10, 1, 3), 8);
        assert_eq!(track_cut(10, 2, 3), 9);
        assert_eq!(track_cut(10, 3, 3), 0);
    }

    #[test]
    fn test_track_cut_neg() {
        assert_eq!(track_cut(10, 4, -4), 8);
        assert_eq!(track_cut(10, 7, -4), 1);
        assert_eq!(track_cut(10, 5, -4), 9);
        assert_eq!(track_cut(10, 6, -4), 0);
    }

    #[test]
    fn test_track_increment() {
        assert_eq!(track_increment(10, 4, 3), 2);
        assert_eq!(track_increment(10, 5, 3), 5);
        assert_eq!(track_increment(10, 6, 3), 8);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("input", 10007, 2019, 1), 3589);
    }
}
