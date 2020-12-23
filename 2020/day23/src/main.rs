use day23::aoclib::*;

fn main() {
    println!("Cups from 1 after 100 moves: {}", solve1("input", 100));
    println!(
        "Product of the two cups after 1 after a ton of moves: {}",
        solve2("input", 10_000_000, 1_000_000)
    );
}

struct CupGame {
    cups: Vec<usize>,
    cur: usize,
    num_cups: usize,
}

impl CupGame {
    fn from(input: &str, num_cups: usize) -> CupGame {
        let mut cups = vec![0; num_cups + 1];

        let input: Vec<_> = input
            .trim()
            .chars()
            .map(|x| x.to_digit(10).unwrap() as usize)
            .collect();

        for i in 1..input.len() {
            cups[input[i - 1]] = input[i];
        }
        if num_cups > 9 {
            cups[input[8]] = 10;

            for i in 11..cups.len() {
                cups[i - 1] = i;
            }
            cups[num_cups] = input[0];
        } else {
            cups[input[8]] = input[0];
        }

        CupGame {
            cups,
            cur: input[0],
            num_cups,
        }
    }

    fn move_cups(&mut self) {
        let out_cups = [
            self.cups[self.cur],
            self.cups[self.cups[self.cur]],
            self.cups[self.cups[self.cups[self.cur]]],
        ];

        let new_next = self.cups[self.cups[self.cups[self.cups[self.cur]]]];

        let mut dest = self.decrement_dest(self.cur);
        while out_cups.contains(&dest) {
            dest = self.decrement_dest(dest);
        }

        self.cups[out_cups[2]] = self.cups[dest];
        self.cups[dest] = self.cups[self.cur];
        self.cups[self.cur] = new_next;

        self.cur = self.cups[self.cur];
    }

    fn play_for(&mut self, moves: u64) {
        for _ in 0..moves {
            self.move_cups();
        }
    }

    fn result(&self) -> String {
        let mut result = String::new();
        let mut cur_val = self.cups[1];
        while cur_val != 1 {
            result.push_str(format!("{}", cur_val).as_str());
            cur_val = self.cups[cur_val];
        }
        result
    }

    fn decrement_dest(&self, d: usize) -> usize {
        if d - 1 == 0 {
            self.num_cups
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

fn solve2(fname: &str, moves: u64, cups: usize) -> usize {
    let input = read_file(fname).unwrap();
    let mut cupgame = CupGame::from(input.as_str(), cups);
    cupgame.play_for(moves);
    let c1 = cupgame.cups[1];
    let c2 = cupgame.cups[c1];
    c1 * c2
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test", 10), String::from("92658374"));
    assert_eq!(solve1("test", 100), String::from("67384529"));
}

#[test]
fn test_game_from() {
    let game = CupGame::from(read_file("test").unwrap().as_str(), 12);
    assert_eq!(game.cups, vec![0, 2, 5, 8, 6, 4, 7, 10, 9, 1, 11, 12, 3]);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test", 10_000_000, 1_000_000), 149245887792);
}
