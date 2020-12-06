use std::collections::HashSet;

use day6::aoclib::*;

fn main() {
    let groups = Groups::from(read_file("input").unwrap());

    println!("Sum of Yes answers (anyone): {}", groups.solve1());

    println!("Sum of Yes answers (everyone): {}", groups.solve2());
}

struct Groups {
    anyone_answered: Vec<HashSet<char>>,
    everyone_answered: Vec<HashSet<char>>,
}

impl Groups {
    fn from(desc: String) -> Groups {
        let mut anyone_answered = Vec::new();
        let mut everyone_answered = Vec::new();
        anyone_answered.push(HashSet::new());
        everyone_answered.push(String::from("abcdefghijklmnopqrstuvwxyz").chars().collect());

        for line in desc.lines() {
            if line.trim() == "" {
                anyone_answered.push(HashSet::new());
                everyone_answered
                    .push(String::from("abcdefghijklmnopqrstuvwxyz").chars().collect());
            } else {
                let all_answers_this_line: HashSet<char> = line.chars().collect();
                for c in line.chars() {
                    anyone_answered.last_mut().unwrap().insert(c);
                }
                let mut cur_everyone: HashSet<char> = everyone_answered.pop().unwrap();
                cur_everyone = cur_everyone
                    .intersection(&all_answers_this_line)
                    .cloned()
                    .collect();
                everyone_answered.push(cur_everyone);
            }
        }

        Groups {
            anyone_answered,
            everyone_answered,
        }
    }

    fn solve1(&self) -> u64 {
        self.anyone_answered.iter().map(|x| x.len() as u64).sum()
    }

    fn solve2(&self) -> u64 {
        self.everyone_answered.iter().map(|x| x.len() as u64).sum()
    }
}

#[test]
fn test_solve1() {
    assert_eq!(Groups::from(read_file("test").unwrap()).solve1(), 11);
}

#[test]
fn test_solve2() {
    assert_eq!(Groups::from(read_file("test").unwrap()).solve2(), 6);
}
