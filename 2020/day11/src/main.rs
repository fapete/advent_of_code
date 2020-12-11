use std::fmt::Display;

use day11::aoclib::*;

fn main() {
    let mut grid = Grid::from(read_file("input").unwrap().as_str());
    println!("Occupied seats predicted: {}", solve1(&mut grid));
    let mut grid = Grid::from(read_file("input").unwrap().as_str());
    println!(
        "Occupied seats predicted with new model: {}",
        solve2(&mut grid)
    );
}

#[derive(Debug, Copy, Clone)]
enum Tile {
    Floor,
    Seat(bool), // Parameter: Occupied
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Tile::Floor => '.',
            Tile::Seat(true) => '#',
            Tile::Seat(false) => 'L',
        };

        write!(f, "{}", c)
    }
}

impl Tile {
    fn is_empty(&self) -> bool {
        match self {
            Tile::Seat(x) => !x,
            Tile::Floor => panic!("Floor is neither empty nor occupied"),
        }
    }

    fn is_seat(&self) -> bool {
        match self {
            Tile::Seat(_) => true,
            _ => false,
        }
    }

    fn flip(self) -> Tile {
        match self {
            Tile::Floor => Tile::Floor,
            Tile::Seat(a) => Tile::Seat(!a),
        }
    }
}

type Pos = (isize, isize);

#[derive(Debug)]
struct Grid {
    tiles: Vec<Tile>,
    width: usize,
    changed: bool,
}

impl Grid {
    fn from(desc: &str) -> Grid {
        let tiles: Vec<Tile> = desc
            .chars()
            .filter(|x| *x != '\n')
            .map(|x| {
                if x == '.' {
                    Tile::Floor
                } else if x == 'L' {
                    Tile::Seat(false)
                } else {
                    Tile::Seat(true)
                }
            })
            .collect();
        let width = desc.find("\n").unwrap();
        assert!(tiles.len() % width == 0); // Sanity check: if this fails, the width is incorrect.

        Grid {
            tiles,
            width,
            changed: true,
        }
    }

    fn get(&self, (x, y): Pos) -> Tile {
        if x < 0 || y < 0 || x >= self.width as isize || y >= self.width as isize {
            Tile::Floor
        } else {
            if let Some(t) = self.tiles.get(y as usize * self.width + x as usize) {
                *t
            } else {
                Tile::Floor
            }
        }
    }

    fn neighbours(&self, (x, y): Pos) -> Vec<Tile> {
        vec![
            self.get((x - 1, y - 1)),
            self.get((x, y - 1)),
            self.get((x + 1, y - 1)),
            self.get((x - 1, y)),
            self.get((x + 1, y)),
            self.get((x - 1, y + 1)),
            self.get((x, y + 1)),
            self.get((x + 1, y + 1)),
        ]
    }

    fn next_visible(&self, p: Pos) -> Vec<Tile> {
        vec![
            self.get_toward(p, (-1, -1)),
            self.get_toward(p, (0, -1)),
            self.get_toward(p, (1, -1)),
            self.get_toward(p, (-1, 0)),
            self.get_toward(p, (1, 0)),
            self.get_toward(p, (-1, 1)),
            self.get_toward(p, (0, 1)),
            self.get_toward(p, (1, 1)),
        ]
    }

    fn get_toward(&self, (mut x, mut y): Pos, (dx, dy): (isize, isize)) -> Tile {
        x += dx;
        y += dy;
        while let Tile::Floor = self.get((x, y)) {
            x += dx;
            y += dy;
            if x < 0 || y < 0 || x >= self.width as isize || y >= self.width as isize {
                return Tile::Floor;
            }
        }
        self.get((x, y))
    }

    fn convert_to_pos(&self, i: usize) -> Pos {
        ((i % self.width) as isize, (i / self.width) as isize)
    }

    fn compute_round1(&mut self) {
        let mut new_tiles = Vec::new();
        self.changed = false;
        for i in 0..self.tiles.len() {
            let tile = self.get(self.convert_to_pos(i));
            if tile.is_seat() {
                let neighbours = self.neighbours(self.convert_to_pos(i));
                let occupied_neighbors = neighbours
                    .iter()
                    .filter(|x| x.is_seat() && !x.is_empty())
                    .count();

                if tile.is_empty() && occupied_neighbors == 0 {
                    new_tiles.push(tile.flip());
                    self.changed = true;
                } else if !tile.is_empty() && occupied_neighbors >= 4 {
                    new_tiles.push(tile.flip());
                    self.changed = true;
                } else {
                    new_tiles.push(tile)
                }
            } else {
                new_tiles.push(tile);
            }
        }
        self.tiles = new_tiles;
    }

    fn compute_round2(&mut self) {
        let mut new_tiles = Vec::new();
        self.changed = false;
        for i in 0..self.tiles.len() {
            let tile = self.get(self.convert_to_pos(i));
            if tile.is_seat() {
                let neighbours = self.next_visible(self.convert_to_pos(i));
                let occupied_neighbors = neighbours
                    .iter()
                    .filter(|x| x.is_seat() && !x.is_empty())
                    .count();

                if tile.is_empty() && occupied_neighbors == 0 {
                    new_tiles.push(tile.flip());
                    self.changed = true;
                } else if !tile.is_empty() && occupied_neighbors >= 5 {
                    new_tiles.push(tile.flip());
                    self.changed = true;
                } else {
                    new_tiles.push(tile)
                }
            } else {
                new_tiles.push(tile);
            }
        }
        self.tiles = new_tiles;
    }

    fn count_occupied(&self) -> u64 {
        self.tiles
            .iter()
            .filter(|x| x.is_seat() && !x.is_empty())
            .count() as u64
    }

    fn print(&self) {
        let mut chars = 0;
        for t in self.tiles.iter() {
            eprint!("{}", t);
            chars += 1;
            if chars == self.width {
                eprint!("\n");
                chars = 0;
            }
        }
        eprint!("\n");
    }
}

fn solve1(grid: &mut Grid) -> u64 {
    while grid.changed {
        grid.compute_round1();
    }
    grid.count_occupied()
}

fn solve2(grid: &mut Grid) -> u64 {
    while grid.changed {
        grid.compute_round2();
    }
    grid.count_occupied()
}

#[test]
fn test_solve1() {
    assert_eq!(
        solve1(&mut Grid::from(read_file("test").unwrap().as_str())),
        37
    )
}

#[test]
fn test_solve2() {
    assert_eq!(
        solve2(&mut Grid::from(read_file("test").unwrap().as_str())),
        26
    )
}

#[test]
fn test_visible_chairs() {
    let g1 = Grid::from(read_file("test_vis1").unwrap().as_str());
    assert_eq!(
        g1.next_visible((3, 4))
            .iter()
            .filter(|x| x.is_seat() && !x.is_empty())
            .count(),
        8
    );

    let g2 = Grid::from(".............\n.L.L.#.#.#.#.\n.............");
    assert_eq!(
        g2.next_visible((1, 1))
            .iter()
            .filter(|x| x.is_seat() && !x.is_empty())
            .count(),
        0
    );
    assert_eq!(
        g2.next_visible((1, 1))
            .iter()
            .filter(|x| x.is_seat())
            .count(),
        1
    );
    assert_eq!(
        g2.next_visible((3, 1))
            .iter()
            .filter(|x| x.is_seat() && !x.is_empty())
            .count(),
        1
    );
    assert_eq!(
        g2.next_visible((3, 1))
            .iter()
            .filter(|x| x.is_seat())
            .count(),
        2
    );

    let g3 = Grid::from(".##.##.\n#.#.#.#\n##...##\n...L...\n##...##\n#.#.#.#\n.##.##.");
    assert_eq!(
        g3.next_visible((3, 3))
            .iter()
            .filter(|x| x.is_seat())
            .count(),
        0
    );
}
