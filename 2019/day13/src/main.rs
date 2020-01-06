use day13::intcode_computer::Machine;
use std::cmp::Ordering;
use std::fmt;
use std::{thread, time};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let character = match self {
            Tile::Empty => '.',
            Tile::Wall => 'X',
            Tile::Block => '#',
            Tile::Paddle => '_',
            Tile::Ball => '*',
        };
        write!(f, "{}", character)
    }
}

struct ArcadeCabinet {
    screen: Vec<Vec<Tile>>,
    ball_pos: isize,
    paddle_pos: isize,
    cpu: Machine,
    score: isize,
}

impl ArcadeCabinet {
    fn new(cpu: Machine) -> ArcadeCabinet {
        ArcadeCabinet {
            cpu,
            screen: Vec::new(),
            score: 0,
            ball_pos: 0,
            paddle_pos: 0,
        }
    }

    fn print_screen(&mut self) {
        while let Some((x, y, t)) = self.get_output_from_cpu() {
            if self.screen.len() > y {
                if self.screen[y].len() > x {
                    self.screen[y][x] = t;
                } else {
                    self.screen[y].resize(x + 1, Tile::Empty);
                    self.screen[y][x] = t;
                }
            } else {
                self.screen.resize(y + 1, Vec::new());
                self.screen[y].resize(x + 1, Tile::Empty);
                self.screen[y][x] = t;
            }
        }
        print!("{}[2J", 27 as char);
        println!("{}", self);
    }

    fn run_game(&mut self) {
        self.cpu.run();
        while !self.cpu.has_halted() {
            thread::sleep(time::Duration::from_millis(50));
            self.print_screen();
            print!("{}[2J", 27 as char);
            println!("{}", self);

            if self.cpu.is_waiting() {
                let joystick = match self.ball_pos.cmp(&self.paddle_pos) {
                    Ordering::Greater => {
                        self.paddle_pos += 1;
                        1
                    }
                    Ordering::Equal => 0,
                    Ordering::Less => {
                        self.paddle_pos -= 1;
                        -1
                    }
                };

                self.cpu.add_input(joystick);
                self.cpu.cont();
            }
        }
        self.print_screen();
    }

    fn get_output_from_cpu(&mut self) -> Option<(usize, usize, Tile)> {
        let t = self.cpu.pop_output();
        let y = self.cpu.pop_output();
        let x = self.cpu.pop_output();
        match t {
            None => None,
            Some(i) => {
                let x = x.unwrap();
                let y = y.unwrap();
                if (x, y) != (-1, 0) {
                    let t = match i {
                        0 => Tile::Empty,
                        1 => Tile::Wall,
                        2 => Tile::Block,
                        3 => {
                            if self.paddle_pos == 0 {
                                self.paddle_pos = x;
                            }
                            Tile::Paddle
                        }
                        4 => {
                            self.ball_pos = x;
                            Tile::Ball
                        }
                        _ => panic!("Invalid tile type returned!"),
                    };
                    Some((x as usize, y as usize, t))
                } else {
                    self.score = i;
                    None
                }
            }
        }
    }

    fn count_tile_type(&self, t: Tile) -> usize {
        self.screen
            .iter()
            .map(|x| x.iter().filter(|y| **y == t).count())
            .sum()
    }
}

impl fmt::Display for ArcadeCabinet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut output = String::from(format!(
            "Score: {}\tPaddles: {}\tBlocks: {}\n",
            self.score,
            self.count_tile_type(Tile::Paddle),
            self.count_tile_type(Tile::Block),
        ));
        for row in &self.screen {
            for tile in row {
                output.push_str(&format!("{}", tile));
            }
            output.push('\n');
        }
        write!(f, "{}", output)
    }
}

fn main() {
    let mut arcade = ArcadeCabinet::new(get_computer());
    arcade.cpu.set_tape_pos(0, 2);
    arcade.run_game();
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
