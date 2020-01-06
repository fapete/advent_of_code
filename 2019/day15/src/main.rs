use day15::intcode_computer::Machine;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::{thread, time};

type Path = VecDeque<Direction>;
type Grid = HashMap<(isize, isize), Tile>;
type GridPos = (isize, isize);

#[derive(PartialEq, Eq, Debug)]
enum Tile {
    Wall,
    Empty,
    Oxygen(u32),
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Tile::Wall => '#',
                Tile::Empty => '.',
                Tile::Oxygen(_) => 'O',
            }
        )
    }
}

#[derive(Clone, Copy, Debug)]
enum Direction {
    East,
    West,
    North,
    South,
}

impl Direction {
    fn to_int(&self) -> isize {
        match self {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }

    fn from_int(i: isize) -> Direction {
        match i {
            1 => Direction::North,
            2 => Direction::South,
            3 => Direction::West,
            4 => Direction::East,
            _ => panic!("Unknown Direction!"),
        }
    }

    fn invert(self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
            Direction::East => Direction::West,
        }
    }

    fn position_change(&self) -> (isize, isize) {
        match self {
            Direction::North => (0, 1),
            Direction::South => (0, -1),
            Direction::West => (-1, 0),
            Direction::East => (1, 0),
        }
    }
}

fn end_pos_of(path: &Path) -> GridPos {
    let mut pos_x = 0;
    let mut pos_y = 0;
    for dir in path {
        let (dx, dy) = dir.position_change();
        pos_x += dx;
        pos_y += dy;
    }
    (pos_x, pos_y)
}

struct RepairRobot {
    cpu: Machine,
    grid: Grid,
    pos: GridPos,
    cur_path: Path,
    path_queue: VecDeque<Path>,
    positions_visited: HashSet<GridPos>,
    oxygen_pos: Option<GridPos>,
}

impl RepairRobot {
    fn new(cpu: Machine) -> RepairRobot {
        let mut visited = HashSet::new();
        visited.insert((0, 0));
        let mut grid = Grid::new();
        grid.insert((0, 0), Tile::Empty);
        RepairRobot {
            cpu,
            grid,
            pos: (0, 0),
            cur_path: Path::new(),
            path_queue: VecDeque::new(),
            positions_visited: visited,
            oxygen_pos: None,
        }
    }

    fn walk_path(&mut self, path: &mut Path) -> isize {
        match path.pop_front() {
            None => self.cpu.pop_output().unwrap(),
            Some(i) => {
                // we can ignore previous output, position already known
                self.cpu.pop_output();
                self.cpu.add_input(i.to_int());
                self.cpu.cont();
                // cur_path is used for backtracking, so push_front the inverse.
                self.cur_path.push_front(i.invert());
                self.walk_path(path)
            }
        }
    }

    fn backtrack(&mut self) {
        while let Some(i) = self.cur_path.pop_front() {
            self.cpu.add_input(i.to_int());
            self.cpu.cont();
            // we can ignore this output, position already known
            self.cpu.pop_output();
        }
    }

    fn add_to_path_queue(&mut self, mut init: Path, output: isize) {
        match output {
            0 => {
                self.grid.insert(end_pos_of(&init), Tile::Wall);
                init.pop_back();
                self.pos = end_pos_of(&init);
                // Remove last step from cur_path, since we walked into wall
                self.cur_path.pop_front();
            }
            1 => {
                self.pos = end_pos_of(&init);
                self.grid.insert(end_pos_of(&init), Tile::Empty);
                for i in 1..5 {
                    let mut new_path = init.clone();
                    new_path.push_back(Direction::from_int(i));
                    if !self.positions_visited.contains(&end_pos_of(&new_path)) {
                        self.positions_visited.insert(end_pos_of(&new_path));
                        self.path_queue.push_back(new_path);
                    }
                }
            }
            2 => {
                self.pos = end_pos_of(&init);
                self.oxygen_pos = Some(end_pos_of(&init));
                self.grid.insert(end_pos_of(&init), Tile::Oxygen(0));
                for i in 1..5 {
                    let mut new_path = init.clone();
                    new_path.push_back(Direction::from_int(i));
                    if !self.positions_visited.contains(&end_pos_of(&new_path)) {
                        self.positions_visited.insert(end_pos_of(&new_path));
                        self.path_queue.push_back(new_path);
                    }
                }
            }
            _ => panic!("Invalid output!"),
        }
    }

    fn search_for_oxygen(&mut self) -> usize {
        // Init
        self.add_to_path_queue(Path::new(), 1);
        self.cpu.run();
        loop {
            self._print_grid();
            let path = self.path_queue.pop_front().unwrap();
            let out = self.walk_path(&mut path.clone());
            if out == 2 {
                // Found the oxygen system, cur_path is a shortest path there.
                self.add_to_path_queue(path, 2);
                break self.cur_path.len();
            } else {
                // Not the oxygen system => backtrack to start, add new paths.
                self.add_to_path_queue(path, out);
                self.backtrack();
            }
        }
    }

    fn fully_explore(&mut self) {
        while let Some(path) = self.path_queue.pop_front() {
            self._print_grid();
            let out = self.walk_path(&mut path.clone());
            self.add_to_path_queue(path, out);
            self.backtrack();
        }
    }

    fn _print_grid(&self) {
        thread::sleep(time::Duration::from_millis(5));
        print!("{}[2J", 27 as char); // Clear screen
                                     //        let max_x = self.grid.keys().max_by(|x, y| x.0.cmp(&y.0)).unwrap().0;
                                     //        let min_x = self.grid.keys().min_by(|x, y| x.0.cmp(&y.0)).unwrap().0;
                                     //        let max_y = self.grid.keys().max_by(|x, y| x.1.cmp(&y.1)).unwrap().1;
                                     //        let min_y = self.grid.keys().min_by(|x, y| x.1.cmp(&y.1)).unwrap().1;

        for y in (-20..21).rev() {
            for x in -20..21 {
                if (x, y) == self.pos {
                    print!("R");
                } else {
                    if self.grid.contains_key(&(x, y)) {
                        print!("{}", self.grid[&(x, y)]);
                    } else {
                        print!(" ");
                    }
                }
            }
            print!("\n");
        }
    }
}

fn oxygenize_grid(grid: &mut Grid, oxygen_pos: GridPos) {
    let mut to_visit = VecDeque::new();
    for adj in adjacent_cells(oxygen_pos) {
        to_visit.push_back((adj, 1));
    }
    while let Some((pos, i)) = to_visit.pop_front() {
        print_grid(&grid);
        grid.entry(pos).and_modify(|x| {
            *x = match x {
                Tile::Empty => {
                    for adj in adjacent_cells(pos) {
                        to_visit.push_back((adj, i + 1))
                    }
                    Tile::Oxygen(i)
                }
                Tile::Wall => Tile::Wall,
                Tile::Oxygen(j) => Tile::Oxygen(*j),
            }
        });
    }
}

fn print_grid(grid: &Grid) {
    thread::sleep(time::Duration::from_millis(5));
    print!("{}[2J", 27 as char); // Clear screen

    for y in (-20..21).rev() {
        for x in -20..21 {
            if grid.contains_key(&(x, y)) {
                print!("{}", grid[&(x, y)]);
            } else {
                print!(" ");
            }
        }
        print!("\n");
    }
}

fn max_dist_oxygen(grid: &Grid) -> u32 {
    let mut max = 0u32;
    for v in grid.values() {
        if let Tile::Oxygen(i) = v {
            if max < *i {
                max = *i;
            }
        }
    }
    max
}

fn adjacent_cells((px, py): GridPos) -> Vec<GridPos> {
    vec![(px, py + 1), (px, py - 1), (px + 1, py), (px - 1, py)]
}

fn main() {
    let mut robot = RepairRobot::new(get_computer());
    // part 1:
    println!("Steps to oxygen: {}", robot.search_for_oxygen());
    thread::sleep(time::Duration::from_secs(2));

    // part 2:
    robot.fully_explore();
    oxygenize_grid(&mut robot.grid, robot.oxygen_pos.unwrap());
    println!(
        "Steps until oxygen is full: {}",
        max_dist_oxygen(&robot.grid)
    );
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
