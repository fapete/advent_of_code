use std::collections::HashMap;

#[derive(Debug)]
struct Resource {
    produced: isize,
    consumed: isize,
}

impl Resource {
    fn new(produced: isize, consumed: isize) -> Resource {
        Resource { produced, consumed }
    }

    fn consume(&mut self, amount: isize) -> isize {
        if self.produced - self.consumed >= amount {
            self.consumed += amount;
            0
        } else {
            let need_to_produce = amount - (self.produced - self.consumed);
            self.consumed = self.produced;
            need_to_produce
        }
    }

    fn update(&mut self, add_produced: isize, add_consumed: isize) {
        self.produced += add_produced;
        self.consumed += add_consumed;
    }
}

#[derive(Debug)]
struct ProductionRule {
    resulting_quantity: isize,
    consumes: Vec<(String, isize)>,
}

impl ProductionRule {
    fn new(resulting_quantity: isize, consumes: Vec<(String, isize)>) -> ProductionRule {
        ProductionRule {
            resulting_quantity,
            consumes,
        }
    }

    fn required_to_produce(&self, amount: isize) -> isize {
        if amount % self.resulting_quantity == 0 {
            amount / self.resulting_quantity
        } else {
            amount / self.resulting_quantity + 1
        }
    }

    fn produces_for(&self, amount: isize) -> isize {
        self.resulting_quantity * self.required_to_produce(amount)
    }

    fn consumes_for(&self, amount: isize) -> Vec<(String, isize)> {
        self.consumes
            .iter()
            .cloned()
            .map(|x| (x.0, x.1 * self.required_to_produce(amount)))
            .collect()
    }
}

#[derive(Debug)]
struct Inventory {
    production_rules: HashMap<String, ProductionRule>,
    resources: HashMap<String, Resource>,
    required_ore: isize,
}

impl Inventory {
    fn new(production_rules: HashMap<String, ProductionRule>) -> Inventory {
        Inventory {
            production_rules,
            resources: HashMap::new(),
            required_ore: 0,
        }
    }

    fn produce(&mut self, resource: String, mut amount_required: isize) {
        match self.resources.get_mut(&resource) {
            Some(r) => amount_required = r.consume(amount_required),
            None => {
                self.resources.insert(resource.clone(), Resource::new(0, 0));
            }
        }
        self.resources.get_mut(&resource).unwrap().update(
            self.production_rules[&resource].produces_for(amount_required),
            amount_required,
        );
        for (r, a) in self.production_rules[&resource].consumes_for(amount_required) {
            if &r != "ORE" {
                self.produce(r, a);
            } else {
                self.required_ore += a;
            }
        }
    }
}

fn parse_resource_grammar(input: &str) -> HashMap<String, ProductionRule> {
    let mut resource_grammar = HashMap::new();
    for line in input.lines() {
        let rule = line.split("=>").collect::<Vec<_>>();
        let required = rule[0]
            .split(",")
            .map(parse_resource)
            .collect::<Vec<(String, isize)>>();
        let produces = parse_resource(rule[1]);
        resource_grammar.insert(produces.0, ProductionRule::new(produces.1, required));
    }
    resource_grammar
}

fn parse_resource(r: &str) -> (String, isize) {
    let components = r.trim().split(" ").collect::<Vec<_>>();
    let amount = components[0].parse::<isize>().unwrap();
    let resource_name = String::from(components[1]);
    (resource_name, amount)
}

fn main() {
    let file = include_str!("../input");
    // part 1
    let grammar = parse_resource_grammar(file);
    let mut inv = Inventory::new(grammar);
    inv.produce(String::from("FUEL"), 1);
    println!("Ore required: {}", inv.required_ore);

    // part 2
    let grammar = parse_resource_grammar(file);
    let mut inv = Inventory::new(grammar);
    inv.produce(String::from("FUEL"), 7863863);
    println!("Ore required: {}", inv.required_ore);
}

//#[cfg(test)]
//mod tests {
//    use crate::*;
//
//    #[test]
//    fn correct_result_test1() {
//    }
//}
