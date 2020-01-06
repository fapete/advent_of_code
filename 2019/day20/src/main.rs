use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

#[derive(Clone, Eq, PartialEq, Debug)]
enum Tile {
    Floor,
    Wall,
    Empty,
    InnerPortal(String),
    OuterPortal(String),
    Intersection,
    Source,
    Target,
}

type Grid = HashMap<GridPos, Tile>;
type GridPos = (usize, usize);

#[derive(Debug)]
struct Maze {
    grid: Grid,
    portals_inner_to_outer: HashMap<String, GridPos>,
    portals_outer_to_inner: HashMap<String, GridPos>,
    source: GridPos,
}

impl Maze {
    fn from_file(file: &str) -> Maze {
        let grid = convert_to_tiles(read_input(file));
        let mut portals_inner_to_outer = HashMap::new();
        let mut portals_outer_to_inner = HashMap::new();
        let mut source = (0, 0);
        for (p, t) in &grid {
            match t {
                Tile::OuterPortal(n) => {
                    portals_inner_to_outer.insert(n.clone(), *p);
                }
                Tile::InnerPortal(n) => {
                    portals_outer_to_inner.insert(n.clone(), *p);
                }
                Tile::Source => source = *p,
                _ => (),
            }
        }
        Maze {
            grid,
            portals_inner_to_outer,
            portals_outer_to_inner,
            source,
        }
    }

    fn shortest_path(&self) -> u32 {
        let mut to_visit: VecDeque<((usize, usize), u32)> = VecDeque::new();
        to_visit.push_back((self.source, 0));
        let mut seen = HashSet::new();

        loop {
            if let Some((p, i)) = to_visit.pop_front() {
                //                eprintln!("Visiting: {:?}", p);
                match &self.grid[&p] {
                    Tile::InnerPortal(n) => {
                        let to = &self.portals_inner_to_outer.get(n).unwrap();
                        if !seen.contains(*to) {
                            to_visit.push_back((**to, i + 1));
                        }
                    }
                    Tile::OuterPortal(n) => {
                        let to = &self.portals_outer_to_inner.get(n).unwrap();
                        if !seen.contains(*to) {
                            to_visit.push_back((**to, i + 1));
                        }
                    }
                    Tile::Target => {
                        break i;
                    }
                    _ => (),
                }

                for np in adjacent_walkable(&self.grid, p)
                    .iter()
                    .filter(|x| !seen.contains(*x))
                {
                    to_visit.push_back((*np, i + 1));
                }
                seen.insert(p);
            }
        }
    }

    fn shortest_path_layered(&self) -> u32 {
        let mut to_visit: VecDeque<(GridPos, u32, u32)> = VecDeque::new();
        to_visit.push_back((self.source, 0, 0));
        let mut seen = HashSet::new();

        //        eprintln!("{:?}", self.grid);
        loop {
            if let Some((p, i, j)) = to_visit.pop_front() {
                //                eprintln!("Visiting: {:?}, steps: {}, layer: {}", p, i, j);
                match &self.grid[&p] {
                    Tile::InnerPortal(n) => {
                        let to = &self.portals_inner_to_outer.get(n).unwrap();
                        if !seen.contains(&(**to, j + 1)) {
                            to_visit.push_back((**to, i + 1, j + 1));
                        }
                    }
                    Tile::OuterPortal(n) => {
                        let to = &self.portals_outer_to_inner.get(n).unwrap();
                        if j > 0 && !seen.contains(&(**to, j - 1)) {
                            to_visit.push_back((**to, i + 1, j - 1));
                        }
                    }
                    Tile::Target => {
                        if j == 0 {
                            break i;
                        }
                    }
                    _ => (),
                }
                for np in adjacent_walkable(&self.grid, p)
                    .iter()
                    .filter(|x| !seen.contains(&(**x, j)))
                {
                    to_visit.push_back((*np, i + 1, j));
                }
                seen.insert((p, j));
            }
        }
    }
}

fn read_input(file: &str) -> Vec<Vec<char>> {
    let mut res = Vec::new();
    let input = fs::read_to_string(file).unwrap();
    for line in input.lines() {
        res.push(Vec::new());
        for c in line.chars() {
            res.last_mut().unwrap().push(c);
        }
    }
    res
}

fn adjacent(grid: &Vec<Vec<char>>, (x, y): (usize, usize)) -> Vec<char> {
    vec![
        grid[(y - 1)][x],
        grid[y][(x - 1)],
        grid[(y + 1)][x],
        grid[y][(x + 1)],
    ]
}

fn convert_to_tiles(grid: Vec<Vec<char>>) -> HashMap<(usize, usize), Tile> {
    let mut res = HashMap::new();
    for y in 1..grid.len() - 1 {
        for x in 1..grid[y].len() - 1 {
            match grid[y][x] {
                ' ' => {
                    res.insert((x, y), Tile::Empty);
                }
                '.' => {
                    let adj = adjacent(&grid, (x, y));
                    if adj.iter().filter(|x| **x == '.').count() > 2 {
                        res.insert((x, y), Tile::Intersection);
                    } else {
                        if adj.iter().filter(|x| x.is_ascii_alphabetic()).count() > 0 {
                            let name = find_portal_name(&grid, (x, y));
                            if name == "AA" {
                                res.insert((x, y), Tile::Source);
                            } else {
                                if name == "ZZ" {
                                    res.insert((x, y), Tile::Target);
                                } else {
                                    if x == 2
                                        || y == 2
                                        || x == grid[y].len() - 3
                                        || y == grid.len() - 3
                                    {
                                        res.insert((x, y), Tile::OuterPortal(name));
                                    } else {
                                        res.insert((x, y), Tile::InnerPortal(name));
                                    }
                                }
                            }
                        } else {
                            res.insert((x, y), Tile::Floor);
                        }
                    }
                }
                '#' => {
                    res.insert((x, y), Tile::Wall);
                }
                _ => {
                    if adjacent(&grid, (x, y))
                        .iter()
                        .filter(|x| **x == ' ')
                        .count()
                        > 2
                    {
                        res.insert((x, y), Tile::Empty);
                    }
                }
            }
        }
    }
    res
}

fn find_portal_name(grid: &Vec<Vec<char>>, (x, y): (usize, usize)) -> String {
    if grid[y + 1][x].is_ascii_alphabetic() {
        format!("{}{}", grid[y + 1][x], grid[y + 2][x])
    } else if grid[y - 1][x].is_ascii_alphabetic() {
        format!("{}{}", grid[y - 2][x], grid[y - 1][x])
    } else if grid[y][x - 1].is_ascii_alphabetic() {
        format!("{}{}", grid[y][x - 2], grid[y][x - 1])
    } else {
        format!("{}{}", grid[y][x + 1], grid[y][x + 2])
    }
}

fn adjacent_walkable(
    grid: &HashMap<(usize, usize), Tile>,
    (x, y): (usize, usize),
) -> Vec<(usize, usize)> {
    vec![(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]
        .iter()
        .cloned()
        .filter(|p| {
            if let Some(t) = grid.get(p) {
                match t {
                    Tile::Wall => false,
                    Tile::Empty => false,
                    _ => true,
                }
            } else {
                false
            }
        })
        .collect()
}

fn part1(file: &str) -> u32 {
    let maze = Maze::from_file(file);
    maze.shortest_path()
}

fn part2(file: &str) -> u32 {
    let maze = Maze::from_file(file);
    maze.shortest_path_layered()
}

fn main() {
    println!("Part 1: {}", part1("input"));
    println!("Part 2: {}", part2("input"));
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_solve() {
        assert_eq!(part1("smallex"), 23);
        assert_eq!(part1("ex"), 58);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("smallex"), 26);
        assert_eq!(part2("p2ex"), 396);
    }
}
