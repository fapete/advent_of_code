use std::collections::HashMap;

use day15::aoclib::*;

fn main() {
    println!(
        "The 2020th Number is: {}",
        solve_for(&read_file("input").unwrap().trim(), 2020)
    );
    println!(
        "The 30000000th Number is: {}",
        solve_for(&read_file("input").unwrap().trim(), 30000000)
    );
}

struct NumberGame {
    turn: u64,
    last_spoken_on: HashMap<u64, Vec<u64>>,
    last_number: u64,
}

impl From<&str> for NumberGame {
    fn from(numbers: &str) -> Self {
        let numbers: Vec<_> = numbers
            .split(",")
            .map(|x| x.parse::<u64>().unwrap())
            .collect();
        let turn = numbers.len() as u64;
        let mut last_spoken = HashMap::new();
        for n in numbers.iter().enumerate() {
            last_spoken.insert(*n.1, vec![n.0 as u64 + 1]);
        }

        NumberGame {
            turn,
            last_spoken_on: last_spoken,
            last_number: *numbers.last().unwrap(),
        }
    }
}

impl NumberGame {
    fn next(&mut self) {
        let new_num;
        if self.last_spoken_on.get(&self.last_number).unwrap().len() == 1 {
            // Number has not been spoken before
            new_num = 0;
        } else {
            // Number has been spoken before
            let last_turn = self
                .last_spoken_on
                .get(&self.last_number)
                .unwrap()
                .iter()
                .rev()
                .skip(1)
                .next()
                .unwrap();
            new_num = self.turn - last_turn;
        }
        if self.turn % 1000000 == 0 {
            eprintln!("{}", self.turn);
        }

        self.turn += 1;
        let turn = self.turn;
        self.last_number = new_num;
        self.last_spoken_on
            .entry(new_num)
            .and_modify(|x| x.push(turn))
            .or_insert(vec![self.turn]);
    }

    fn get_turn(&self) -> u64 {
        self.turn
    }

    fn get_last_number(&self) -> u64 {
        self.last_number
    }
}

fn solve_for(input: &str, i: u64) -> u64 {
    let mut game = NumberGame::from(input);

    while game.get_turn() < i {
        game.next();
    }
    return game.get_last_number();
}

#[test]
fn test_solve1() {
    assert_eq!(solve_for("0,3,6", 2020), 436);
    assert_eq!(solve_for("1,3,2", 2020), 1);
    assert_eq!(solve_for("2,1,3", 2020), 10);
    assert_eq!(solve_for("1,2,3", 2020), 27);
    assert_eq!(solve_for("2,3,1", 2020), 78);
    assert_eq!(solve_for("3,2,1", 2020), 438);
    assert_eq!(solve_for("3,1,2", 2020), 1836);
}

#[test]
fn test_solve2() {
    assert_eq!(solve_for("0,3,6", 30000000), 175594);
    assert_eq!(solve_for("1,3,2", 30000000), 2578);
    assert_eq!(solve_for("2,1,3", 30000000), 3544142);
    assert_eq!(solve_for("1,2,3", 30000000), 261214);
    assert_eq!(solve_for("2,3,1", 30000000), 6895259);
    assert_eq!(solve_for("3,2,1", 30000000), 18);
    assert_eq!(solve_for("3,1,2", 30000000), 362);
}
