use std::collections::HashMap;

use day19::aoclib::*;

fn main() {
    println!("Matching Strings: {}", solve1("input"));
}

#[derive(Debug)]
struct Grammar {
    p: HashMap<u64, Vec<RHS>>,
}

impl Grammar {
    fn parse_grammar<'a, T>(rules: &mut T) -> Grammar
    where
        T: Iterator<Item = &'a str>,
    {
        let mut p: HashMap<u64, Vec<RHS>> = HashMap::new();
        for line in rules {
            let lhs_rhs: Vec<_> = line.split(":").map(|x| x.trim()).collect();
            let lhs = lhs_rhs[0].parse::<u64>().unwrap();
            let rhs: Vec<_> = lhs_rhs[1].split("|").map(RHS::from).collect();
            p.insert(lhs, rhs);
        }
        Grammar { p }
    }

    // input is already almost in CNF, but there are a few rules with only one NT on the RHS
    fn normalize(&mut self) {
        loop {
            let offending_nts: Vec<u64> = self
                .p
                .iter()
                .filter(|(_, rhs)| {
                    rhs.iter()
                        .any(|rule| rule.symbols.len() < 2 && rule.symbols[0].is_nt())
                })
                .map(|(l, _)| *l)
                .collect();
            if offending_nts.len() == 0 {
                break;
            }

            for nt in offending_nts.iter() {
                let mut new_rhs: Vec<RHS> = Vec::new();
                for rule in self.p.get(nt).unwrap().iter() {
                    if rule.symbols.len() < 2 {
                        for new_rule in self.p.get(&rule.symbols[0].get_nt_val().unwrap()).unwrap()
                        {
                            new_rhs.push(new_rule.clone());
                        }
                    }
                }
                self.p.entry(*nt).and_modify(|x| *x = new_rhs);
            }
        }
    }

    fn cyk(&self, i: &str, s: u64) -> bool {
        let mut derivable_from: Vec<Vec<Vec<bool>>> = Vec::new();
        derivable_from.resize(i.len(), Vec::new());
        for v in derivable_from.iter_mut() {
            v.resize(i.len(), Vec::with_capacity(self.p.keys().count()));
            for v1 in v.iter_mut() {
                v1.resize(self.p.keys().count(), false);
            }
        }

        for s in 0..i.len() {
            let a_s: char = i.chars().skip(s).next().unwrap();
            for (v, rhs) in self
                .p
                .iter()
                .filter(|(_, r)| r.iter().all(|x| is_terminal(&x.symbols)))
            {
                if rhs[0].symbols[0].get_t_val().unwrap() == a_s {
                    derivable_from[0][s][*v as usize] = true;
                }
            }
        }

        for l in 1..i.len() {
            for s in 0..i.len() - l {
                for p in 0..l {
                    for (a, rhs) in self
                        .p
                        .iter()
                        .filter(|(_, r)| r.iter().all(|x| !is_terminal(&x.symbols)))
                    {
                        for rule in rhs {
                            let b = rule.symbols[0].get_nt_val().unwrap() as usize;
                            let c = rule.symbols[1].get_nt_val().unwrap() as usize;
                            if derivable_from[p][s][b] && derivable_from[l - (p + 1)][s + p + 1][c]
                            {
                                derivable_from[l][s][*a as usize] = true;
                            }
                        }
                    }
                }
            }
        }
        //eprintln!("Final for {}: {:?}", i, derivable_from);
        derivable_from[i.len() - 1][0][s as usize]
    }
}

fn parse_grammer_from_input<'a, T>(lines: &mut T) -> Grammar
where
    T: Iterator<Item = &'a str>,
{
    Grammar::parse_grammar(&mut lines.take_while(|line| line.trim() != ""))
}

fn solve1(fname: &str) -> u64 {
    let input = read_file(fname).unwrap();
    let mut lines = input.lines();
    let mut grammar = parse_grammer_from_input(&mut lines);
    grammar.normalize();

    let mut c = 0;
    for m in lines.filter(|x| grammar.cyk(*x, 0)) {
        eprintln!("{}", m);
        c += 1;
    }
    c
}

#[derive(Debug, Clone)]
struct RHS {
    symbols: Vec<Sym>,
}

impl From<&str> for RHS {
    fn from(rule: &str) -> RHS {
        let mut symbols = Vec::new();
        for sym in rule.split_ascii_whitespace() {
            symbols.push(match sym.parse::<u64>() {
                Ok(x) => Sym::NT(x),
                Err(_) => Sym::T(sym.chars().skip(1).take(1).next().unwrap()),
            });
        }
        RHS { symbols }
    }
}

fn is_terminal(rule: &Vec<Sym>) -> bool {
    match rule[0] {
        Sym::NT(_) => false,
        Sym::T(_) => true,
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Sym {
    NT(u64),
    T(char),
}

impl Sym {
    fn is_nt(&self) -> bool {
        match self {
            Sym::NT(_) => true,
            Sym::T(_) => false,
        }
    }

    fn get_nt_val(&self) -> Option<u64> {
        match self {
            Sym::NT(x) => Some(*x),
            _ => None,
        }
    }

    fn get_t_val(&self) -> Option<char> {
        match self {
            Sym::T(x) => Some(*x),
            _ => None,
        }
    }
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 2);
    assert_eq!(solve1("test3"), 8);
    assert_eq!(solve1("test4"), 2);
}
