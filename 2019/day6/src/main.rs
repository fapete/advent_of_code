use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug)]
struct Node {
    name: String,
    orbited_by: Vec<Rc<RefCell<Node>>>,
    count: Rc<RefCell<u32>>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Node {
    pub fn new(name: &str) -> Node {
        Node {
            name: String::from(name),
            orbited_by: Vec::new(),
            count: Rc::new(RefCell::new(0)),
        }
    }

    pub fn add_orbit(&mut self, node: &Rc<RefCell<Node>>) {
        self.orbited_by.push(Rc::clone(node));
    }

    pub fn compute_counts_from_here(&self) {
        for c in self.orbited_by.iter() {
            *(c.borrow().count.borrow_mut()) += *self.count.borrow() + 1;
            c.borrow().compute_counts_from_here()
        }
    }

    pub fn sum_counts(&self) -> u32 {
        let mut sum = 0;
        for c in self.orbited_by.iter() {
            sum += c.borrow().sum_counts()
        }
        *self.count.borrow() + sum
    }

    pub fn dist(&self, src: &str, target: &str) -> (u32, bool) {
        if self.name == src || self.name == target {
            (0, true)
        } else {
            let dists = self
                .orbited_by
                .iter()
                .map(|x| x.borrow().dist(src, target))
                .collect::<Vec<_>>();
            let still_counting = dists.iter().filter(|(_, counting)| *counting).count();
            if still_counting == 2 {
                (dists.iter().map(|(i, _)| i).sum(), false)
            } else if still_counting == 1 {
                (dists.iter().map(|(i, _)| i).sum::<u32>() + 1, true)
            } else {
                (dists.iter().map(|(i, _)| i).sum(), false)
            }
        }
    }
}

fn main() {
    let input = include_str!("../input")
        .lines()
        .map(|x| x.split(')').collect())
        .collect::<Vec<Vec<&str>>>();

    let mut node_dict = HashMap::new();

    for orbit in input {
        let center = orbit[0];
        let object = orbit[1];

        // Add both nodes if not already existing
        node_dict
            .entry(center)
            .or_insert(Rc::new(RefCell::new(Node::new(center))));
        node_dict
            .entry(object)
            .or_insert(Rc::new(RefCell::new(Node::new(object))));

        let center = node_dict.get(&center).unwrap();
        let object = node_dict.get(&object).unwrap();
        center.borrow_mut().add_orbit(object);
    }

    let root = node_dict.get("COM").unwrap();
    root.borrow().compute_counts_from_here();
    // Part 1:
    println!("Total number of Orbits: {}", root.borrow().sum_counts());

    // Part 2:
    println!("Steps to Santa: {}", root.borrow().dist("SAN", "YOU").0);
}
