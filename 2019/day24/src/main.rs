use std::collections::HashSet;
use std::fmt;

type Grid = Vec<Tile>;

#[derive(Debug)]
enum Tile {
    Empty,
    Bug,
    Rec,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Tile::Empty => write!(f, "."),
            Tile::Bug => write!(f, "#"),
            Tile::Rec => write!(f, "?"),
        }
    }
}

impl Tile {
    fn is_bug(&self) -> bool {
        match self {
            Tile::Bug => true,
            _ => false,
        }
    }
}

struct BugsRec {
    grids_above: Vec<Grid>,
    grids_below: Vec<Grid>,
}

impl Bugs {
    fn from(input_grid: &str) -> Bugs {
        let mut grid = Grid::new();
        for line in input_grid.lines() {
            for c in line.chars() {
                match c {
                    '.' => grid.push(Tile::Empty),
                    '#' => grid.push(Tile::Bug),
                    _ => panic!("Unknown Tile!"),
                }
            }
        }
        grid[12] = Tile::Rec;
        let mut grids_above = Vec::new();
        grids_above.push(grid);
        Bugs {
            grids_above,
            grids_below: Vec::new(),
        }
    }

    fn tile_at(&self, level: i32, x: u8, y: u8) -> Option<&Tile> {
        if x > 4 || y > 4 {
            None
        } else {
            if level >= 0 {
                self.grids_above[level as usize].get((y * 5 + x) as usize)
            } else {
                self.grids_below[(-1 * level - 1) as usize].get((y * 5 + x) as usize)
            }
        }
    }

    fn num_adjacent_bugs(&self, level: i32, x: i8, y: i8) -> u8 {
        let mut count = vec![(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]
            .iter()
            .filter(|(x, y)| *x >= 0 && *y >= 0)
            .map(|(x, y)| {
                self.tile_at(level, *x as u8, *y as u8)
                    .filter(|t| t.is_bug())
            })
            .filter(|x| x.is_some())
            .count() as u8;
        if level == 0 {
            if self.grids_above.len() > 1 {
            } else {
            }
        } else {
            if level > 0 {
            } else {
            }
        }
        if level + 1 == self.grids_above.len() {}
    }

    fn generation(&mut self) {
        let counts = self
            .grid
            .iter()
            .enumerate()
            .map(|(i, _)| self.num_adjacent_bugs(i as i8 % 5, i as i8 / 5))
            .collect::<Vec<_>>();
        self.grid = self
            .grid
            .iter()
            .zip(counts)
            .map(|(t, c)| self.evaluate_infestation(t, c))
            .collect();
    }

    fn evaluate_infestation(&self, t: &Tile, num_adjacent: u8) -> Tile {
        match t {
            Tile::Empty => {
                if num_adjacent == 1 || num_adjacent == 2 {
                    Tile::Bug
                } else {
                    Tile::Empty
                }
            }
            Tile::Bug => {
                if num_adjacent != 1 {
                    Tile::Empty
                } else {
                    Tile::Bug
                }
            }
        }
    }

    fn biodiversity(&self) -> u64 {
        let mut r = 0;
        for t in self.grid.iter().rev() {
            match t {
                Tile::Empty => r <<= 1,
                Tile::Bug => r = (r << 1) | 1,
            }
        }
        r
    }

    fn _print_grid(&self) {
        for y in 0..5 {
            for x in 0..5 {
                print!("{}", self.tile_at(x, y).unwrap());
            }
            print!("\n");
        }
    }

    fn run_until_repeat(&mut self) -> u64 {
        loop {
            let bio = self.biodiversity();
            if !self.seen_layouts.contains(&bio) {
                self.seen_layouts.insert(bio);
                self.generation();
            } else {
                break bio;
            }
        }
    }
}

struct Bugs {
    grid: Grid,
    seen_layouts: HashSet<u64>,
}

impl Bugs {
    fn from(input_grid: &str) -> Bugs {
        let mut grid = Vec::new();
        for line in input_grid.lines() {
            for c in line.chars() {
                match c {
                    '.' => grid.push(Tile::Empty),
                    '#' => grid.push(Tile::Bug),
                    _ => panic!("Unknown Tile!"),
                }
            }
        }
        Bugs {
            grid,
            seen_layouts: HashSet::new(),
        }
    }

    fn tile_at(&self, x: u8, y: u8) -> Option<&Tile> {
        if x > 4 || y > 4 {
            None
        } else {
            self.grid.get((y * 5 + x) as usize)
        }
    }

    fn num_adjacent_bugs(&self, x: i8, y: i8) -> u8 {
        vec![(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]
            .iter()
            .filter(|(x, y)| *x >= 0 && *y >= 0)
            .map(|(x, y)| self.tile_at(*x as u8, *y as u8).filter(|t| t.is_bug()))
            .filter(|x| x.is_some())
            .count() as u8
    }

    fn generation(&mut self) {
        let counts = self
            .grid
            .iter()
            .enumerate()
            .map(|(i, _)| self.num_adjacent_bugs(i as i8 % 5, i as i8 / 5))
            .collect::<Vec<_>>();
        self.grid = self
            .grid
            .iter()
            .zip(counts)
            .map(|(t, c)| self.evaluate_infestation(t, c))
            .collect();
    }

    fn evaluate_infestation(&self, t: &Tile, num_adjacent: u8) -> Tile {
        match t {
            Tile::Empty => {
                if num_adjacent == 1 || num_adjacent == 2 {
                    Tile::Bug
                } else {
                    Tile::Empty
                }
            }
            Tile::Bug => {
                if num_adjacent != 1 {
                    Tile::Empty
                } else {
                    Tile::Bug
                }
            }
        }
    }

    fn biodiversity(&self) -> u64 {
        let mut r = 0;
        for t in self.grid.iter().rev() {
            match t {
                Tile::Empty => r <<= 1,
                Tile::Bug => r = (r << 1) | 1,
            }
        }
        r
    }

    fn _print_grid(&self) {
        for y in 0..5 {
            for x in 0..5 {
                print!("{}", self.tile_at(x, y).unwrap());
            }
            print!("\n");
        }
    }

    fn run_until_repeat(&mut self) -> u64 {
        loop {
            let bio = self.biodiversity();
            if !self.seen_layouts.contains(&bio) {
                self.seen_layouts.insert(bio);
                self.generation();
            } else {
                break bio;
            }
        }
    }
}

fn main() {
    let mut bugs = Bugs::from(include_str!("../input"));
    println!("Biodiversity of first repeat: {}", bugs.run_until_repeat());
}

#[cfg(test)]
mod tests {
    use crate::*;

    fn test_grid1() -> &'static str {
        ".....\n.....\n.....\n#....\n.#..."
    }

    #[test]
    fn test_biodiversity() {
        let surface = Bugs::from(test_grid1());
        assert_eq!(surface.biodiversity(), 2129920);
    }
}
