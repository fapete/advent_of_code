use regex::Regex;
use std::collections::HashMap;

use day24::aoclib::*;

fn main() {
    println!("Black Tiles: {}", solve1("input"));
    println!("Black Tiles after 100 rounds: {}", solve2("input"));
}

type Pos3 = (i64, i64, i64);
#[derive(Clone, Copy, Debug)]
enum TileType {
    White,
    Black,
}

#[derive(Clone, Copy, Debug)]
struct Tile {
    colour: TileType,
    to_flip: bool,
}

impl Tile {
    fn new_black() -> Tile {
        Tile {
            colour: TileType::Black,
            to_flip: false,
        }
    }

    fn new_white() -> Tile {
        Tile {
            colour: TileType::White,
            to_flip: false,
        }
    }

    fn flip(&mut self) {
        self.colour.flip();
    }

    fn is_black(&self) -> bool {
        self.colour.is_black()
    }

    fn mark_flip(&mut self) {
        self.to_flip = true;
    }
}

impl TileType {
    fn flip(&mut self) {
        match self {
            TileType::White => *self = TileType::Black,
            TileType::Black => *self = TileType::White,
        }
    }

    fn is_black(&self) -> bool {
        match self {
            TileType::White => false,
            TileType::Black => true,
        }
    }
}

#[derive(Debug)]
struct HexGrid {
    grid: HashMap<Pos3, Tile>,
    min_x: i64,
    max_x: i64,
    min_y: i64,
    max_y: i64,
    min_z: i64,
    max_z: i64,
}

impl HexGrid {
    fn new() -> HexGrid {
        HexGrid {
            grid: HashMap::new(),
            min_x: 0,
            max_x: 0,
            min_y: 0,
            max_y: 0,
            min_z: 0,
            max_z: 0,
        }
    }

    fn flip_tile_at(&mut self, pos: Pos3) {
        self.grid
            .entry(pos)
            .and_modify(|x| x.flip())
            .or_insert(Tile::new_black());
        self.update_dimensions(pos);
    }

    fn update_dimensions(&mut self, (x, y, z): Pos3) {
        if x < self.min_x {
            self.min_x = x;
        }
        if x > self.max_x {
            self.max_x = x;
        }
        if y < self.min_y {
            self.min_y = y;
        }
        if y > self.max_y {
            self.max_y = y;
        }
        if z < self.min_z {
            self.min_z = z;
        }
        if z > self.max_z {
            self.max_z = z;
        }
    }

    fn count_black(&self) -> u64 {
        self.grid.values().filter(|x| x.is_black()).count() as u64
    }

    fn get(&self, pos: &Pos3) -> Tile {
        match self.grid.get(&pos) {
            Some(t) => *t,
            None => Tile::new_white(),
        }
    }

    fn mark_flip_at(&mut self, pos: &Pos3) {
        self.grid
            .entry(*pos)
            .and_modify(|x| x.mark_flip())
            .or_insert_with(|| {
                let mut t = Tile::new_white();
                t.mark_flip();
                t
            });
    }

    fn count_black_neighbours(&self, pos: Pos3) -> u64 {
        let additions = [
            (1, 0, -1),
            (1, -1, 0),
            (0, -1, 1),
            (-1, 0, 1),
            (-1, 1, 0),
            (0, 1, -1),
        ];

        additions
            .iter()
            .map(|x| self.get(&add_pos(*x, pos)))
            .filter(|t| t.is_black())
            .count() as u64
    }

    fn life_round(&mut self) {
        //eprintln!("{:?}", self);
        for x in self.min_x - 1..self.max_x + 2 {
            for y in self.min_y - 1..self.max_y + 2 {
                for z in self.min_z - 1..self.max_z + 2 {
                    let black_neighbours = self.count_black_neighbours((x, y, z));
                    match self.get(&(x, y, z)).colour {
                        TileType::White => {
                            if black_neighbours == 2 {
                                self.mark_flip_at(&(x, y, z));
                                self.update_dimensions((x, y, z));
                            }
                        }
                        TileType::Black => {
                            if black_neighbours == 0 || black_neighbours > 2 {
                                self.mark_flip_at(&(x, y, z));
                                self.update_dimensions((x, y, z));
                            }
                        }
                    }
                }
            }
        }
        for tile in self.grid.values_mut() {
            if tile.to_flip {
                tile.flip();
                tile.to_flip = false;
            }
        }
    }
}

fn solve1(fname: &str) -> u64 {
    let input = read_file(fname).unwrap();
    let mut grid = HexGrid::new();

    for line in input.trim().lines() {
        grid.flip_tile_at(parse_str_coordinate(line));
    }

    grid.count_black()
}

fn solve2(fname: &str) -> u64 {
    let input = read_file(fname).unwrap();
    let mut grid = HexGrid::new();

    for line in input.trim().lines() {
        grid.flip_tile_at(parse_str_coordinate(line));
    }

    for i in 1..101 {
        grid.life_round();
        if i < 10 || i % 10 == 0 {
            eprintln!("Day {}: {}", i, grid.count_black());
        }
    }

    grid.count_black()
}

fn add_pos((x1, y1, z1): Pos3, (x2, y2, z2): Pos3) -> Pos3 {
    (x1 + x2, y1 + y2, z1 + z2)
}

fn parse_str_coordinate(coordinate: &str) -> Pos3 {
    let mut pos = (0, 0, 0);
    let re = Regex::new(r"(ne|nw|e|w|se|sw)").unwrap();
    for cap in re.captures_iter(coordinate) {
        let d_pos = match &cap[1] {
            "ne" => (1, 0, -1),
            "e" => (1, -1, 0),
            "se" => (0, -1, 1),
            "sw" => (-1, 0, 1),
            "w" => (-1, 1, 0),
            "nw" => (0, 1, -1),
            _ => panic!("Invalid coordinate parsed!"),
        };
        pos = add_pos(pos, d_pos);
    }
    pos
}

#[test]
fn test_parse_str_coordinates() {
    assert_eq!(parse_str_coordinate("nwwswee"), (0, 0, 0));
    assert_eq!(parse_str_coordinate("esew"), (0, -1, 1));
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 10);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test"), 2208);
}
