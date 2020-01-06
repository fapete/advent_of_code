use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
struct LinePosition {
    x: i32,
    y: i32,
}

impl LinePosition {
    fn from(x: i32, y: i32) -> LinePosition {
        LinePosition { x, y }
    }

    fn manhattan_from_origin(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }
}

fn main() {
    let file = include_str!("../input");

    let mut wires: Vec<HashSet<LinePosition>> = Vec::new();
    let mut steps_at_pos: Vec<HashMap<LinePosition, i32>> = Vec::new();
    for line in file.lines() {
        let mut x = 0;
        let mut y = 0;
        let mut stepcount = 0;
        wires.push(HashSet::new());
        steps_at_pos.push(HashMap::new());
        for (d, steps) in line
            .split(',')
            .map(|x| x.split_at(1))
            .map(|(x, y)| (x, y.parse::<i32>().unwrap()))
        {
            for _ in 0..steps {
                stepcount += 1;
                match d {
                    "R" => {
                        x += 1;
                    }
                    "L" => {
                        x -= 1;
                    }
                    "U" => {
                        y += 1;
                    }
                    "D" => {
                        y -= 1;
                    }
                    _ => panic!("invalid direction"),
                }
                wires.last_mut().unwrap().insert(LinePosition::from(x, y));
                steps_at_pos
                    .last_mut()
                    .unwrap()
                    .entry(LinePosition::from(x, y))
                    .or_insert(stepcount);
                //                if !steps_at_pos
                //                    .last()
                //                    .unwrap()
                //                    .contains_key(&LinePosition::from(x, y))
                //                {
                //                    steps_at_pos
                //                        .last_mut()
                //                        .unwrap()
                //                        .insert(LinePosition::from(x, y), stepcount);
                //                }
            }
        }
    }

    let mut intersections: Vec<&LinePosition> = wires[0].intersection(&wires[1]).collect();
    // Part 1:
    intersections.sort_unstable_by(|x, y| compare_by_manhattan_distance(x, y));
    println!(
        "Manhattan distance of closest intersection: {} (intersection at {:?})",
        intersections[0].manhattan_from_origin(),
        intersections[0]
    );

    // Part 2:
    let mut step_pairs: Vec<_> = intersections
        .iter()
        .map(|x| (x, steps_at_pos[0][x]))
        .zip(intersections.iter().map(|x| (x, steps_at_pos[1][x])))
        .map(|((lp1, s1), (_, s2))| (lp1, s1 + s2))
        .collect();

    step_pairs.sort_unstable_by(|(_, s1), (_, s2)| s1.cmp(s2));
    println!("Fewest combined steps are: {}", step_pairs[0].1);
}

fn compare_by_manhattan_distance(x: &LinePosition, y: &LinePosition) -> Ordering {
    x.manhattan_from_origin().cmp(&y.manhattan_from_origin())
}
