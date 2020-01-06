use day17::intcode_computer::Machine;
use std::collections::HashMap;
use std::{thread, time};

type GridPos = (isize, isize);
type Tile = char;
type Grid = HashMap<GridPos, Tile>;

struct ASCII {
    grid: Grid,
    computer: Machine,
    grid_dim: GridPos,
}

impl ASCII {
    fn new(computer: Machine) -> ASCII {
        ASCII {
            grid: Grid::new(),
            computer,
            grid_dim: (0, 0),
        }
    }

    fn get_view(&mut self) {
        let mut x = 0;
        let mut y = 0;
        let mut x_max = 0;
        if self.grid_dim == (0, 0) {
            while let Some(i) = self.computer.pop_output_front() {
                let c = char::from(i as u8);
                if c == '\n' {
                    x = 0;
                    y += 1;
                } else {
                    self.grid.insert((x, y), c);
                    x += 1;
                    if x > x_max {
                        x_max = x;
                    }
                }
            }
        } else {
            for y in 0..self.grid_dim.1 {
                self.computer.pop_output_front(); // newline
                for x in 0..self.grid_dim.0 {
                    let c = char::from(self.computer.pop_output_front().unwrap() as u8);
                    self.grid.insert((x, y), c);
                }
            }
        }
        self.grid_dim = (x_max, y - 1);
        self.computer.cont();
    }

    fn adjacent(&self, (x, y): GridPos) -> Vec<Option<&Tile>> {
        vec![
            self.grid.get(&(x - 1, y)),
            self.grid.get(&(x, y - 1)),
            self.grid.get(&(x + 1, y)),
            self.grid.get(&(x, y + 1)),
        ]
    }

    fn find_intersections(&mut self) {
        for y in 1..self.grid_dim.1 - 1 {
            for x in 1..self.grid_dim.0 - 1 {
                if self.adjacent((x, y)).iter().all(|x| *x == Some(&'#')) {
                    self.grid.insert((x, y), 'O');
                }
            }
        }
    }

    fn alignment(&self) -> isize {
        self.grid
            .iter()
            .filter(|(_, c)| **c == 'O')
            .map(|((x, y), _)| x * y)
            .sum::<isize>()
    }

    fn wake_up_robot(&mut self) {
        self.computer.set_tape_pos(0, 2);
        self.computer.run();
    }

    fn program_robot(&mut self) {
        let prog_a = "R,8,L,10,L,12,R,4\n";
        let prog_b = "R,8,L,12,R,4,R,4\n";
        let prog_c = "R,8,L,10,R,8\n";
        let main = "A,B,A,C,A,B,C,B,C,B\n";

        self.computer.set_input(
            format!("{}{}{}{}n\n", main, prog_a, prog_b, prog_c)
                .chars()
                .map(|x| (x as u8) as isize)
                .rev()
                .collect(),
        );
    }

    fn print_grid(&self) {
        print!("{}[2J", 27 as char); // Clear screen

        for y in 0..self.grid_dim.1 {
            for x in 0..self.grid_dim.0 {
                match self.grid.get(&(x, y)) {
                    None => print!(" "),
                    Some(c) => print!("{}", c),
                }
            }
            print!("\n");
        }
    }
}

fn main() {
    let mut camera = ASCII::new(get_computer());
    camera.computer.run();
    camera.get_view();
    camera.find_intersections();
    //    camera.print_grid();
    // part 1:
    println!("Alignment: {}", camera.alignment());

    // part 2:
    let mut camera = ASCII::new(get_computer());
    camera.wake_up_robot();
    camera.program_robot();
    camera.get_view();
    camera.print_grid();
    //    camera.computer.cont();
    while !camera.computer.has_halted() {
        camera.get_view();
        thread::sleep(time::Duration::from_millis(25));
        camera.print_grid();
    }
    println!("Output: {:?}", camera.computer.pop_output());
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
