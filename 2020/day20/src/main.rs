use day20::aoclib::*;
use std::{collections::HashMap, fmt};

fn main() {
    println!("Multiplying the corners gets: {}", solve1("input"));
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Tile {
    Wall,
    Floor,
}

const EAST_EDGE: [Pos; 10] = [
    (0, 9),
    (1, 9),
    (2, 9),
    (3, 9),
    (4, 9),
    (5, 9),
    (6, 9),
    (7, 9),
    (8, 9),
    (9, 9),
];
const SOUTH_EDGE: [Pos; 10] = [
    (9, 0),
    (9, 1),
    (9, 2),
    (9, 3),
    (9, 4),
    (9, 5),
    (9, 6),
    (9, 7),
    (9, 8),
    (9, 9),
];
const WEST_EDGE: [Pos; 10] = [
    (0, 0),
    (1, 0),
    (2, 0),
    (3, 0),
    (4, 0),
    (5, 0),
    (6, 0),
    (7, 0),
    (8, 0),
    (9, 0),
];
const NORTH_EDGE: [Pos; 10] = [
    (0, 0),
    (0, 1),
    (0, 2),
    (0, 3),
    (0, 4),
    (0, 5),
    (0, 6),
    (0, 7),
    (0, 8),
    (0, 9),
];

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tile::Wall => write!(f, "#"),
            Tile::Floor => write!(f, "."),
        }
    }
}

type Pos = (usize, usize);

#[derive(Debug, Clone)]
struct Grid {
    id: u64,
    map: Vec<Vec<Tile>>,
    rotate: u64,
    flipped: bool,
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut res = String::new();

        for row in 0..10 {
            res.push_str("\n");
            for col in 0..10 {
                res.push_str(format!("{}", self.get((row, col))).as_str());
            }
        }
        write!(f, "{}", res.as_str())
    }
}

impl Grid {
    fn from_lines<'a, T>(lines: &mut T) -> Option<(Grid, u64)>
    where
        T: Iterator<Item = &'a str>,
    {
        let maybe_tile_id = lines.next();
        if maybe_tile_id.is_some() {
            let mut map: Vec<Vec<Tile>> = Vec::with_capacity(10);
            while let Some(line) = lines.next() {
                if line.trim() == "" {
                    break;
                }
                map.push(Vec::with_capacity(10));
                let cur_line = map.last_mut().unwrap();
                for c in line.chars() {
                    if c == '#' {
                        cur_line.push(Tile::Wall);
                    } else {
                        cur_line.push(Tile::Floor);
                    }
                }
            }

            let tile_id = maybe_tile_id
                .unwrap()
                .split_ascii_whitespace()
                .skip(1)
                .next()
                .unwrap()
                .trim_end_matches(':')
                .parse::<u64>()
                .unwrap();
            Some((
                Grid {
                    id: tile_id,
                    map,
                    rotate: 0,
                    flipped: false,
                },
                tile_id,
            ))
        } else {
            None
        }
    }

    fn get(&self, (x, y): Pos) -> Tile {
        let (mut new_x, mut new_y) = (x, y);
        if self.rotate == 90 {
            new_x = y;
            new_y = 9 - x;
        } else if self.rotate == 180 {
            new_x = 9 - x;
            new_y = 9 - y;
        } else if self.rotate == 270 {
            new_x = 9 - y;
            new_y = x;
        }

        if self.flipped {
            new_x = 9 - new_x;
        }

        self.map[new_x][new_y]
    }

    fn rotate(&mut self) {
        self.rotate = self.rotate + 90 % 360;
    }

    fn flip(&mut self) {
        self.flipped = !self.flipped;
    }

    fn get_north_edge(&self) -> Vec<Tile> {
        self.get_edge(&NORTH_EDGE)
    }

    fn get_east_edge(&self) -> Vec<Tile> {
        self.get_edge(&EAST_EDGE)
    }

    fn get_south_edge(&self) -> Vec<Tile> {
        self.get_edge(&SOUTH_EDGE)
    }

    fn get_west_edge(&self) -> Vec<Tile> {
        self.get_edge(&WEST_EDGE)
    }

    fn get_edge(&self, edge: &[Pos; 10]) -> Vec<Tile> {
        edge.iter().map(|x| self.get(*x)).collect()
    }

    fn matches_on_north(&self, other: &mut Grid) -> bool {
        for _ in 0..4 {
            if matching_edge(self.get_north_edge(), other.get_south_edge()) {
                return true;
            }
            other.flip();
            if matching_edge(self.get_north_edge(), other.get_south_edge()) {
                return true;
            }
            other.flip();
            other.rotate();
        }
        false
    }

    fn matches_on_south(&self, other: &mut Grid) -> bool {
        for _ in 0..4 {
            if matching_edge(self.get_south_edge(), other.get_north_edge()) {
                return true;
            }
            other.flip();
            if matching_edge(self.get_south_edge(), other.get_north_edge()) {
                return true;
            }
            other.flip();
            other.rotate();
        }
        false
    }

    fn matches_on_east(&self, other: &mut Grid) -> bool {
        for _ in 0..4 {
            if matching_edge(self.get_east_edge(), other.get_west_edge()) {
                return true;
            }
            other.flip();
            if matching_edge(self.get_east_edge(), other.get_west_edge()) {
                return true;
            }
            other.flip();
            other.rotate();
        }
        false
    }

    fn matches_on_west(&self, other: &mut Grid) -> bool {
        for _ in 0..4 {
            if matching_edge(self.get_west_edge(), other.get_east_edge()) {
                return true;
            }
            other.flip();
            if matching_edge(self.get_west_edge(), other.get_east_edge()) {
                return true;
            }
            other.flip();
            other.rotate();
        }
        false
    }

    fn rotate_to_upper_left(&mut self, unique_edges: [u64; 4]) {
        if unique_edges[0] == 0 && unique_edges[1] == 0 {
            self.rotate();
        } else if unique_edges[1] == 0 && unique_edges[2] == 0 {
            self.rotate();
            self.rotate();
        } else if unique_edges[2] == 0 && unique_edges[3] == 0 {
            self.rotate();
            self.rotate();
            self.rotate();
        }
    }
}

fn matching_edge(edge1: Vec<Tile>, edge2: Vec<Tile>) -> bool {
    edge1 == edge2
}

#[derive(Debug)]
struct MapJigsaw {
    grids: HashMap<u64, Grid>,
    arrangement: Vec<Vec<u64>>,
}

impl MapJigsaw {
    fn arrange(&mut self) {}
}

fn find_corner_in(all_grids: &Vec<Grid>, from_idx: usize) -> Option<u64> {
    for grid in all_grids.iter().skip(from_idx) {
        let mut edge_matches: [u64; 4] = [0; 4]; // 0: above, 1: right, 2: below, 3: left
        for other in all_grids.iter().cloned() {
            if grid.id != other.id {
                edge_matches[0] += if grid.matches_on_north(&mut other.clone()) {
                    eprintln!("{} matches {} above", grid.id, other.id);
                    1
                } else {
                    0
                };
                edge_matches[1] += if grid.matches_on_east(&mut other.clone()) {
                    eprintln!("{} matches {} right", grid.id, other.id);
                    1
                } else {
                    0
                };
                edge_matches[2] += if grid.matches_on_south(&mut other.clone()) {
                    eprintln!("{} matches {} below", grid.id, other.id);
                    1
                } else {
                    0
                };
                edge_matches[3] += if grid.matches_on_west(&mut other.clone()) {
                    eprintln!("{} matches {} left", grid.id, other.id);
                    1
                } else {
                    0
                };
            }
        }
        if edge_matches.iter().filter(|x| **x == 0).count() == 2 {
            return Some(grid.id);
        }
    }
    None
}

fn solve1(infile: &str) -> u64 {
    let f = read_file(infile).unwrap();
    let jigsaw = MapJigsaw::from(f.as_str());
    eprintln!("{:?}", jigsaw.grids.keys());
    let all_grids: Vec<_> = jigsaw.grids.values().cloned().collect();
    let mut result: u64 = 1;
    let mut init = 0;

    loop {
        let maybe_corner = find_corner_in(&all_grids, init);
        match maybe_corner {
            Some(x) => {
                eprintln!("Corner: {}", x);
                result *= x;
                let idx = all_grids
                    .iter()
                    .enumerate()
                    .find(|(_, g)| g.id == x)
                    .unwrap()
                    .0;
                init = idx + 1;
            }
            None => break,
        }
    }

    result
}

impl From<&str> for MapJigsaw {
    fn from(input: &str) -> MapJigsaw {
        let mut grids = HashMap::new();

        let mut lines = input.lines();

        while let Some((grid, tile_id)) = Grid::from_lines(&mut lines) {
            grids.insert(tile_id, grid);
        }

        let side_length = (grids.keys().count() as f64).sqrt() as usize;
        let arrangement = vec![vec![0; side_length]; side_length];

        MapJigsaw { grids, arrangement }
    }
}

#[test]
fn manual_test_grid_parse() {
    let mut jigsaw = MapJigsaw::from(read_file("test").unwrap().as_str());

    let g2311 = jigsaw.grids.get(&2311).unwrap();
    let mut g3079 = jigsaw.grids.get(&3079).unwrap().clone();
    assert!(g2311.matches_on_east(&mut g3079));
    /*
    let mut jigsaw = MapJigsaw::from(read_file("test").unwrap().as_str());
    eprintln!("{:#?}", jigsaw);
    let corner = jigsaw.find_a_corner();
    eprintln!(
        "Corner: {:?} Grid: {}",
        corner,
        jigsaw.grids.get(&corner.0).unwrap()
    );

    jigsaw
        .grids
        .get_mut(&corner.0)
        .unwrap()
        .rotate_to_upper_left(corner.1);
    let g = jigsaw.grids.get(&corner.0).unwrap();
    eprintln!("After rotation: {}", g);
    assert!(g.matches_on_east(&mut jigsaw.grids.get(&2311).unwrap().clone()));
    */
}

#[test]
fn test_rotation() {
    let jigsaw = MapJigsaw::from(read_file("test").unwrap().as_str());

    let g = jigsaw.grids.get(&2729).unwrap().clone();
    let mut g2 = g.clone();

    g2.rotate();

    assert_eq!(g2.rotate, 90);

    eprintln!("g: {}", g);
    eprintln!("g2: {}", g2);

    assert_eq!(g.get_east_edge(), g2.get_north_edge());

    g2.rotate();
    eprintln!("g2: {}", g2);
    let mut g2_west = g2.get_west_edge();
    g2_west.reverse();
    assert_eq!(g.get_east_edge(), g2_west);

    g2.rotate();
    eprintln!("g2: {}", g2);
    let mut g2_south = g2.get_south_edge();
    g2_south.reverse();
    assert_eq!(g.get_east_edge(), g2_south);

    g2.rotate();
    eprintln!("g2: {}", g2);
    assert_eq!(g.get_east_edge(), g2.get_east_edge());
}

#[test]
fn test_flip() {
    let jigsaw = MapJigsaw::from(read_file("test").unwrap().as_str());

    let g = jigsaw.grids.get(&2729).unwrap().clone();
    let mut g2 = g.clone();
    g2.flip();

    eprintln!("{}", g);
    eprintln!("{}", g2);
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 20899048083289);
}
