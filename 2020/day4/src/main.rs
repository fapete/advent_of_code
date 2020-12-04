use day4::aoclib::*;
use regex::Regex;
use std::collections::{HashMap, HashSet};

fn main() {
    println!(
        "Task 1: {}",
        solve1(&parse_passports(read_file("input").unwrap()))
    );

    println!(
        "Task 2: {}",
        solve2(&parse_passports(read_file("input").unwrap()))
    );
}

const REQUIRED_FIELDS: [&str; 7] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
#[derive(Debug)]
struct Passport {
    values: HashMap<String, String>,
}

impl Passport {
    fn new() -> Passport {
        let values = HashMap::new();

        Passport { values }
    }

    fn from(desc: &String) -> Passport {
        let mut p = Passport::new();

        for pair in desc.split_ascii_whitespace() {
            let key_val: Vec<_> = pair.split(':').collect();
            p.values
                .insert(String::from(key_val[0]), String::from(key_val[1]));
        }

        p
    }

    fn has_required_fields(&self) -> bool {
        REQUIRED_FIELDS.iter().all(|x| self.values.contains_key(*x))
    }

    fn is_valid(&self) -> bool {
        if self.has_required_fields() {
            let mut result = true;
            result = result
                && self
                    .values
                    .get("byr")
                    .unwrap()
                    .parse::<u64>()
                    .map_or(false, |x| x >= 1920 && x <= 2002);

            result = result
                && self
                    .values
                    .get("iyr")
                    .unwrap()
                    .parse::<u64>()
                    .map_or(false, |x| x >= 2010 && x <= 2020);

            result = result
                && self
                    .values
                    .get("eyr")
                    .unwrap()
                    .parse::<u64>()
                    .map_or(false, |x| x >= 2020 && x <= 2030);

            let hgt = self.values.get("hgt").unwrap();
            let (num, unit) = hgt.split_at(hgt.len() - 2);
            if unit == "cm" {
                result = result && num.parse::<u64>().map_or(false, |x| x >= 150 && x <= 193);
            } else if unit == "in" {
                result = result && num.parse::<u64>().map_or(false, |x| x >= 59 && x <= 76);
            } else {
                result = false;
            }

            let hcl = self.values.get("hcl").unwrap();
            let hcl_re = Regex::new(r"^#[0-9abcdef]{6}$").unwrap();
            result = result && hcl_re.is_match(hcl);

            let ecl = self.values.get("ecl").unwrap();
            let mut valid_ecl = HashSet::new();
            valid_ecl.insert("amb");
            valid_ecl.insert("blu");
            valid_ecl.insert("brn");
            valid_ecl.insert("gry");
            valid_ecl.insert("grn");
            valid_ecl.insert("hzl");
            valid_ecl.insert("oth");
            result = result && valid_ecl.contains(ecl.as_str());

            let pid = self.values.get("pid").unwrap();
            let pid_re = Regex::new(r"^\d{9}$").unwrap();
            result = result && pid_re.is_match(pid);

            result
        } else {
            false
        }
    }
}

fn parse_passports(desc: String) -> Vec<Passport> {
    let mut ps = Vec::new();

    let mut single_passport = String::new();
    for line in desc.lines() {
        if line.trim() == "" {
            ps.push(Passport::from(&single_passport));
            single_passport = String::new();
        } else {
            single_passport = format!("{} {}", single_passport, line);
        }
    }
    ps.push(Passport::from(&single_passport));

    ps
}

fn solve1(ps: &Vec<Passport>) -> u64 {
    ps.iter()
        .map(|p| p.has_required_fields())
        .filter(|x| *x)
        .count() as u64
}

fn solve2(ps: &Vec<Passport>) -> u64 {
    ps.iter()
        .filter(|p| p.has_required_fields())
        .map(|p| p.is_valid())
        .filter(|x| *x)
        .count() as u64
}

#[test]
fn test_task1() {
    let pports = parse_passports(read_file("test").unwrap());
    assert_eq!(solve1(&pports), 2);
    assert_eq!(pports.len(), 4);
}

#[test]
fn test_task2() {
    let pports_inv = parse_passports(read_file("test2").unwrap());
    let pports_val = parse_passports(read_file("test3").unwrap());
    assert_eq!(solve2(&pports_inv), 0);
    assert_eq!(solve2(&pports_val), 4);
}

#[test]
fn test_regex() {
    let hcl_re = Regex::new(r"^#[1-9abcdef]{6}$").unwrap();
    assert!(hcl_re.is_match("#28df12"));
    assert!(!hcl_re.is_match("#28dj12"));
    assert!(!hcl_re.is_match("#28df1"));
    assert!(!hcl_re.is_match("28df12"));
}
