use day21::aoclib::*;
use regex::Regex;
use std::collections::{HashMap, HashSet};

fn main() {
    let mut ingredients = create_ingredients("input");
    println!(
        "Ingredient occurrences that do not have an allergen: {}",
        solve1(&ingredients)
    );
    println!("Canonical Dangerous List: {}", solve2(&mut ingredients));
}

struct Ingredients {
    ingredient_lists: Vec<HashSet<String>>,
    allergens_to_ingredients: HashMap<String, HashSet<String>>,
}

impl Ingredients {
    fn new() -> Self {
        Ingredients {
            ingredient_lists: Vec::new(),
            allergens_to_ingredients: HashMap::new(),
        }
    }

    fn parse_ingredient_list_line(&mut self, line: &str) {
        let re = Regex::new(r"^(.*)\(contains (.*)\)$").unwrap();
        let parsed_line = re.captures(line).unwrap();

        let ingredients: HashSet<_> = parsed_line[1]
            .split_ascii_whitespace()
            .map(String::from)
            .collect();
        let allergens: Vec<_> = parsed_line[2]
            .split(',')
            .map(|x| String::from(x.trim()))
            .collect();

        for allergen in allergens.iter() {
            self.allergens_to_ingredients
                .entry(allergen.clone())
                .and_modify(|s| {
                    *s = s.intersection(&ingredients).cloned().collect();
                })
                .or_insert(ingredients.clone());
        }
        self.ingredient_lists.push(ingredients.clone());
    }

    fn without_allergens(&self) -> Vec<String> {
        let ingredients_with_allergens = self.with_allergens();
        let all_ingredients = self
            .ingredient_lists
            .iter()
            .fold(HashSet::new(), |x, y| x.union(y).cloned().collect());
        all_ingredients
            .difference(&ingredients_with_allergens)
            .cloned()
            .collect()
    }

    fn with_allergens(&self) -> HashSet<String> {
        self.allergens_to_ingredients
            .values()
            .fold(HashSet::new(), |x, y| x.union(y).cloned().collect())
    }

    fn reduce_ingredients(&mut self) {
        let mut processed_allergens: HashSet<String> = HashSet::new();

        loop {
            let candidate = self
                .allergens_to_ingredients
                .iter()
                .find(|x| x.1.len() == 1 && !processed_allergens.contains(&x.0.clone()));

            match candidate {
                Some((k, list)) => {
                    let k = k.clone();
                    let list = list.clone();
                    for (a, ing) in self.allergens_to_ingredients.iter_mut() {
                        if k != *a {
                            *ing = ing.difference(&list).cloned().collect();
                            processed_allergens.insert(k.clone());
                        }
                    }
                }
                None => break,
            }
        }
    }

    fn canonical_dangerous_list(&self) -> String {
        let mut ingredient_allergen_pairs: Vec<_> = self.allergens_to_ingredients.iter().collect();
        ingredient_allergen_pairs.sort_unstable_by_key(|x| x.0);

        let mut already_seen: HashSet<String> = HashSet::new();
        let mut result = String::new();

        for (_, i) in ingredient_allergen_pairs.iter() {
            for ingredient in i.iter() {
                if !already_seen.contains(ingredient) {
                    result = format!("{},{}", result, ingredient);
                    already_seen.insert(ingredient.clone());
                }
            }
        }

        String::from(result.trim_start_matches(","))
    }
}

fn create_ingredients(fname: &str) -> Ingredients {
    let mut ingredients = Ingredients::new();
    for line in read_file(fname).unwrap().lines() {
        ingredients.parse_ingredient_list_line(line);
    }
    ingredients
}

fn solve2(ingredients: &mut Ingredients) -> String {
    ingredients.reduce_ingredients();
    ingredients.canonical_dangerous_list()
}

fn solve1(ingredients: &Ingredients) -> u64 {
    let no_allergens = ingredients.without_allergens();

    let mut acc = 0;
    for ingredient in no_allergens {
        for list in ingredients.ingredient_lists.iter() {
            acc += if list.contains(&ingredient) { 1 } else { 0 };
        }
    }
    acc
}

#[test]
fn test_solve1() {
    assert_eq!(solve1(&create_ingredients("test")), 5);
}

#[test]
fn test_solve2() {
    assert_eq!(
        solve2(&mut create_ingredients("test")),
        String::from("mxmxvkd,sqjhc,fvjkl")
    );
}
