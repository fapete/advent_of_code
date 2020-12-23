use day23::aoclib::*;

fn main() {
    println!("Cups from 1 after 100 moves: {}", solve1("input", 100));
    println!(
        "Product of the two cups after 1 after a ton of moves: {}",
        solve2("input", 10_000_000, 1_000_000)
    );
}

struct CupGame {
    cups: Vec<u64>,
    cur: usize,
    dest: usize,
    num_cups: usize,
}

impl CupGame {
    fn from(input: &str, num_cups: usize) -> CupGame {
        let mut cups: Vec<_> = input
            .trim()
            .chars()
            .map(|x| x.to_digit(10).unwrap() as u64)
            .collect();
        for i in 10..num_cups + 1 {
            cups.push(i as u64);
        }

        CupGame {
            cups,
            cur: 0,
            dest: 0,
            num_cups,
        }
    }

    fn move_cups(&mut self) {
        let out_cups = [
            self.cups[(self.cur + 1) % self.num_cups],
            self.cups[(self.cur + 2) % self.num_cups],
            self.cups[(self.cur + 3) % self.num_cups],
        ];

        //eprintln!("---- Move ----");
        //eprintln!("Cups: {:?}", self.cups);
        //eprintln!("Current: {}", self.cups[self.cur]);
        //eprintln!("Pick up: {:?}", out_cups);
        //eprintln!("Destination: {}", self.cups[self.dest]);
        //eprintln!("\n");

        let mut cur_to_idx = (self.cur + 1) % self.num_cups;
        let mut cur_from_idx = (self.cur + 4) % self.num_cups;
        // Shift cups between the ones taken out and dest
        while cur_from_idx != self.dest {
            self.cups[cur_to_idx] = self.cups[cur_from_idx];
            cur_to_idx = (cur_to_idx + 1) % self.num_cups;
            cur_from_idx = (cur_from_idx + 1) % self.num_cups;
        }
        // move dest cup
        self.cups[cur_to_idx] = self.cups[cur_from_idx];

        // put cups taken out back in
        for i in 1..4 {
            self.cups[(cur_to_idx + i) % self.num_cups] = out_cups[i - 1];
        }
    }

    fn find_dest(&mut self) {
        let mut dest_value = self.decrement_dest(self.cups[self.cur]);
        //eprintln!("Dest Value: {}", dest_value);
        let mut dest_idx = self.find_idx_of(dest_value);
        while self.is_move_cup(dest_idx) {
            dest_value = self.decrement_dest(dest_value);
            //  eprintln!("Dest Value: {}", dest_value);
            dest_idx = self.find_idx_of(dest_value);
        }
        self.dest = dest_idx;
    }

    fn increment_cur(&mut self) {
        self.cur = (self.cur + 1) % self.num_cups;
    }

    fn is_move_cup(&self, idx: usize) -> bool {
        idx == (self.cur + 1) % self.num_cups
            || idx == (self.cur + 2) % self.num_cups
            || idx == (self.cur + 3) % self.num_cups
    }

    fn find_idx_of(&self, value: u64) -> usize {
        //eprintln!("Looking for index of {}", value);
        self.cups
            .iter()
            .enumerate()
            .find(|(_, x)| **x == value)
            .unwrap()
            .0
    }

    fn play_for(&mut self, moves: u64) {
        for i in 0..moves {
            self.find_dest();
            self.move_cups();
            self.increment_cur();
            if i % 500_000 == 0 {
                println!("Move {}", i);
            }
        }
    }

    fn result(&self) -> String {
        let mut result = String::new();
        let init = self.find_idx_of(1);
        for i in 1..self.num_cups {
            result.push_str(format!("{}", self.cups[(init + i) % self.num_cups]).as_str());
        }
        result
    }
    fn decrement_dest(&self, d: u64) -> u64 {
        if d - 1 == 0 {
            self.num_cups as u64
        } else {
            d - 1
        }
    }
}

fn solve1(fname: &str, moves: u64) -> String {
    let input = read_file(fname).unwrap();
    let mut cupgame = CupGame::from(input.as_str(), 9);
    cupgame.play_for(moves);
    cupgame.result()
}

fn solve2(fname: &str, moves: u64, cups: usize) -> u64 {
    let input = read_file(fname).unwrap();
    let mut cupgame = CupGame::from(input.as_str(), cups);
    cupgame.play_for(moves);
    let one = cupgame.find_idx_of(1);
    let c1 = cupgame.cups[(one + 1) % cups];
    let c2 = cupgame.cups[(one + 2) % cups];
    c1 * c2
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test", 10), String::from("92658374"));
    assert_eq!(solve1("test", 100), String::from("67384529"));
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test", 10_000_000, 1_000_000), 149245887792);
}
