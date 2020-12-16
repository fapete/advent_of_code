use std::collections::HashSet;

use day16::aoclib::*;

fn main() {
    println!("The ticket scanning error rate is {}", solve1("input"));
    println!("The ticket scanning error rate is {}", solve2("input"));
}

#[derive(Debug, Clone)]
struct Ticket {
    numbers: Vec<u64>,
    rules: Vec<Rule>,
}

impl From<&str> for Ticket {
    fn from(ticket_desc: &str) -> Ticket {
        Ticket {
            numbers: ticket_desc
                .split(",")
                .map(|x| x.parse::<u64>().unwrap())
                .collect(),
            rules: Vec::new(),
        }
    }
}

impl Ticket {
    fn completely_invalid_numbers(&self, rules: &Vec<Rule>) -> Vec<u64> {
        let mut result = Vec::new();
        for num in self.numbers.iter() {
            if rules
                .iter()
                .map(|x| x.has_number_in_range(*num))
                .all(|x| !x)
            {
                result.push(*num);
            }
        }
        result
    }

    fn is_invalid(&self, rules: &Vec<Rule>) -> bool {
        self.completely_invalid_numbers(rules).len() > 0
    }

    fn matching_rules(&self, rules: &Vec<Rule>) -> Vec<HashSet<usize>> {
        let mut result = Vec::new();
        for num in self.numbers.iter() {
            result.push(
                rules
                    .iter()
                    .enumerate()
                    .filter(|(_, rule)| rule.has_number_in_range(*num))
                    .map(|(idx, _)| idx)
                    .collect(),
            );
        }
        result
    }
}

#[derive(Debug, Clone)]
struct Rule {
    name: String,
    range1: (u64, u64),
    range2: (u64, u64),
}

impl Rule {
    fn new() -> Rule {
        Rule {
            name: String::from(""),
            range1: (0, 0),
            range2: (0, 0),
        }
    }
    fn has_number_in_range(&self, num: u64) -> bool {
        let (min1, max1) = self.range1;
        let (min2, max2) = self.range2;

        (min1 <= num && num <= max1) || (min2 <= num && num <= max2)
    }
}

impl From<&str> for Rule {
    fn from(rule: &str) -> Rule {
        let name_ranges: Vec<_> = rule.split(":").map(|x| x.trim()).collect();
        let ranges: Vec<_> = name_ranges[1].split(" or ").collect();
        let name = String::from(name_ranges[0]);
        let range1: Vec<_> = ranges[0]
            .split("-")
            .map(|x| x.parse::<u64>().unwrap())
            .collect();
        let range1 = (range1[0], range1[1]);
        let range2: Vec<_> = ranges[1]
            .split("-")
            .map(|x| x.parse::<u64>().unwrap())
            .collect();
        let range2 = (range2[0], range2[1]);

        Rule {
            name,
            range1,
            range2,
        }
    }
}

fn read_rules<'a, T>(lines: &mut T) -> Vec<Rule>
where
    T: Iterator<Item = &'a str>,
{
    lines
        .take_while(|line| line.trim() != "")
        .map(|line| Rule::from(line))
        .collect()
}

fn read_my_ticket<'a, T>(lines: &mut T) -> Ticket
where
    T: Iterator<Item = &'a str>,
{
    lines.next(); // Line that says "your ticket"
    Ticket::from(lines.next().unwrap())
}

fn read_other_tickets<'a, T>(lines: &mut T) -> Vec<Ticket>
where
    T: Iterator<Item = &'a str>,
{
    lines.next(); // Empty Line
    lines.next(); // String "nearby tickets:"
    lines.map(Ticket::from).collect()
}

fn solve1(infile: &str) -> u64 {
    let file_as_string = read_file(infile).unwrap();
    let mut lines = file_as_string.lines();
    let rules = read_rules(&mut lines);
    read_my_ticket(&mut lines);
    let other_tickets = read_other_tickets(&mut lines);

    other_tickets
        .iter()
        .map(|ticket| {
            ticket
                .completely_invalid_numbers(&rules)
                .iter()
                .sum::<u64>()
        })
        .sum()
}

fn solve2(infile: &str) -> u64 {
    let file_as_string = read_file(infile).unwrap();
    let mut lines = file_as_string.lines();
    let rules = read_rules(&mut lines);
    let mut my_ticket = read_my_ticket(&mut lines);
    let mut other_tickets = read_other_tickets(&mut lines);

    other_tickets = other_tickets
        .iter()
        .filter(|x| !x.is_invalid(&rules))
        .cloned()
        .collect();

    let mut matching_rules = other_tickets[0].matching_rules(&rules);
    for ticket in other_tickets.iter() {
        let this_ticket_matches = ticket.matching_rules(&rules);
        for (idx, rule_set) in this_ticket_matches.iter().enumerate() {
            matching_rules[idx] = rule_set
                .intersection(&matching_rules[idx])
                .cloned()
                .collect();
        }
    }

    my_ticket.rules.resize(my_ticket.numbers.len(), Rule::new());

    loop {
        if matching_rules.iter().all(|x| x.len() == 0) {
            break;
        }

        let (idx, set) = matching_rules
            .iter_mut()
            .enumerate()
            .find(|(_, set)| set.len() == 1)
            .unwrap();
        let rule_idx: Vec<_> = set.drain().collect();

        for rule_set in matching_rules.iter_mut() {
            rule_set.remove(&rule_idx[0]);
        }

        my_ticket.rules[idx] = rules[rule_idx[0]].clone();
    }

    eprintln!("{:?}", matching_rules);
    eprintln!("{:?}", my_ticket.rules);

    let mut result = 1;
    for (idx, rule) in my_ticket.rules.iter().enumerate() {
        if rule.name.starts_with("departure") {
            result *= my_ticket.numbers[idx];
        }
    }
    result
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 71);
}
