use day7::aoclib::*;
use std::collections::{HashMap, HashSet};

use petgraph::graphmap::DiGraphMap;
use petgraph::Direction::{Incoming, Outgoing};
use regex::Regex;

fn main() {
    let x = read_rules(read_file("input").unwrap());
    let g = create_graph(&x);
    //println!("{:?}", g);
    let bags_containing_shiny_gold = num_reachable_from(&g, "shiny gold");
    println!("Can reach shiny gold: {}", bags_containing_shiny_gold);
    println!("Bags in shiny gold: {}", bags_in(&g, "shiny gold"));
}

fn num_reachable_from(g: &DiGraphMap<&str, u64>, from: &str) -> u64 {
    let mut todo = vec![from];
    let mut bags = HashSet::new();
    while let Some(n) = todo.pop() {
        for next in g.neighbors_directed(n, Incoming) {
            todo.push(next);
            bags.insert(next);
        }
    }
    bags.len() as u64
}

fn bags_in(g: &DiGraphMap<&str, u64>, from: &str) -> u64 {
    let mut count = 0;
    for n in g.neighbors_directed(from, Outgoing) {
        let weight = g.edge_weight(from, n).unwrap();
        count += weight + weight * bags_in(g, n);
    }
    count
}

fn create_graph(rules: &HashMap<String, Vec<(u64, String)>>) -> DiGraphMap<&str, u64> {
    let mut g = DiGraphMap::new();

    for n in rules.keys() {
        g.add_node(n.as_str());
    }

    for (n, es) in rules.iter() {
        for e in es {
            g.add_edge(n.as_str(), e.1.as_str(), e.0);
        }
    }

    g
}

fn read_rules(desc: String) -> HashMap<String, Vec<(u64, String)>> {
    let mut rules_map = HashMap::new();

    let ls_re = Regex::new(r"^(.*) bags contain (.*)$").unwrap();
    let rs_re = Regex::new(r"^(\d) (.*) bag").unwrap();
    for line in desc.lines() {
        for x in ls_re.captures_iter(line) {
            let left_side = &x[1];
            let right_side = &x[2];
            //print!("{}: ", left_side);
            rules_map.insert(String::from(left_side), Vec::new());
            for var in right_side.split(',') {
                for bags in rs_re.captures_iter(var.trim()) {
                    let mult = &bags[1].parse::<u64>().unwrap();
                    let bag = String::from(&bags[2]);

                    rules_map
                        .get_mut(&String::from(left_side))
                        .unwrap()
                        .push((*mult, bag));
                }
            }
        }
    }

    rules_map
}
