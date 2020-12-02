use std::fs::File;
use std::io::prelude::*;

fn main() {
    let input = read_file("./input").unwrap();
    let valid_count_old = input.lines().map(valid_pw_1).filter(|x| *x).count();
    println!(
        "Number of valid passwords according to old place is {}",
        valid_count_old
    );
    let valid_count_new = input.lines().map(valid_pw_2).filter(|x| *x).count();
    println!(
        "Number of valid passwords according to new place is {}",
        valid_count_new
    );
}

fn valid_pw_1(pw: &str) -> bool {
    let parts: Vec<_> = pw.split(':').collect();
    Policy::from(parts[0]).check_part1(parts[1])
}

fn valid_pw_2(pw: &str) -> bool {
    let parts: Vec<_> = pw.split(':').collect();
    Policy::from(parts[0]).check_part2(parts[1])
}
struct Policy {
    min: u64,
    max: u64,
    c: char,
}

impl Policy {
    fn from(description: &str) -> Policy {
        let parts: Vec<_> = description.split(' ').collect();
        let minmax: Vec<_> = parts[0].split("-").collect();
        Policy {
            min: minmax[0].parse::<u64>().unwrap(),
            max: minmax[1].parse::<u64>().unwrap(),
            c: parts[1].parse::<char>().unwrap(),
        }
    }

    fn check_part1(self, pw: &str) -> bool {
        let char_count = pw.chars().filter(|x| *x == self.c).count();
        self.min <= char_count as u64 && self.max >= char_count as u64
    }

    fn check_part2(self, pw: &str) -> bool {
        let chars: Vec<_> = pw.chars().collect();
        (chars[self.min as usize] == self.c && chars[self.max as usize] != self.c)
            || (chars[self.min as usize] != self.c && chars[self.max as usize] == self.c)
    }
}

fn read_file(fname: &str) -> std::io::Result<String> {
    let mut file = File::open(fname)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    return Ok(content);
}
