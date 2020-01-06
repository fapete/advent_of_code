use day11::intcode_computer::Machine;
use std::collections::HashMap;

#[derive(Debug)]
struct Robot {
    computer: Machine,
    pos: (i32, i32),
    dir: (i32, i32),
    painted_panels: HashMap<(i32, i32), i32>,
}

impl Robot {
    fn new(computer: Machine) -> Robot {
        Robot {
            computer,
            pos: (0, 0),
            dir: (0, 1),
            painted_panels: HashMap::new(),
        }
    }

    fn mv(&mut self) {
        let (dx, dy) = self.dir;
        let (x, y) = self.pos;
        self.pos = (x + dx, y + dy);
    }

    fn turn_left(&mut self) {
        let (dx, dy) = self.dir;
        self.dir = (dx - dy - dx, dy + dx - dy);
    }

    fn turn_right(&mut self) {
        let (dx, dy) = self.dir;
        self.dir = (dx + dy - dx, dy - dx - dy);
    }

    fn move_and_paint(&mut self, init: i32) {
        let colour = self.painted_panels.entry(self.pos).or_insert(init);
        self.computer.add_input(*colour as isize);
        self.computer.cont();
        let turn_signal = self.computer.get_output().pop().unwrap();
        let colour_to_paint = self.computer.get_output().pop().unwrap();
        match colour_to_paint {
            0 => {
                self.painted_panels
                    .entry(self.pos)
                    .and_modify(|e| *e = 0)
                    .or_insert(0);
            }
            1 => {
                self.painted_panels
                    .entry(self.pos)
                    .and_modify(|e| *e = 1)
                    .or_insert(1);
            }
            _ => panic!("Invalid colour returned from computer!"),
        }
        match turn_signal {
            0 => {
                self.turn_left();
            }
            1 => {
                self.turn_right();
            }
            _ => panic!("Invalid turning direction returned from computer!"),
        }
        self.mv();
    }

    fn do_paint(&mut self, init: i32) -> usize {
        while !self.computer.has_halted() {
            self.move_and_paint(init);
        }
        self.number_of_painted_panels()
    }

    fn number_of_painted_panels(&self) -> usize {
        self.painted_panels.len()
    }
}

fn main() {
    // Part 1:
    let mut robot = Robot::new(get_computer());
    let panels_painted = robot.do_paint(0);
    pretty_print_panels(&robot.painted_panels);
    println!("Panels painted: {}", panels_painted);
    // Part 2:
    let mut robot = Robot::new(get_computer());
    robot.do_paint(1);
    pretty_print_panels(&robot.painted_panels);
}

fn pretty_print_panels(panel_colours: &HashMap<(i32, i32), i32>) {
    let mut min_x = 0i32;
    let mut max_x = 0i32;
    let mut min_y = 0i32;
    let mut max_y = 0i32;
    for ((x, y), _) in panel_colours {
        if *x < min_x {
            min_x = *x;
        }
        if *x > max_x {
            max_x = *x;
        }
        if *y < min_y {
            min_y = *y;
        }
        if *y > max_y {
            max_y = *y;
        }
    }

    for j in (min_y..max_y + 1).rev() {
        for i in min_x..max_x + 1 {
            let colour = panel_colours.get(&(i, j));
            match colour {
                None => print!(" "),
                Some(i) => print!("{}", if *i == 0 { " " } else { "#" }),
            }
        }
        print!("\n");
    }
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn robot_correctly_turns_right() {
        let mut robot = Robot::new(get_computer());
        // init: dx = 0, dy = 1;
        assert_eq!(robot.dir, (0, 1));
        robot.turn_right();
        assert_eq!(robot.dir, (1, 0));
        robot.turn_right();
        assert_eq!(robot.dir, (0, -1));
        robot.turn_right();
        assert_eq!(robot.dir, (-1, 0));
        robot.turn_right();
        assert_eq!(robot.dir, (0, 1));
    }

    #[test]
    fn robot_correctly_turns_left() {
        let mut robot = Robot::new(get_computer());
        // init: dx = 0, dy = 1;
        assert_eq!(robot.dir, (0, 1));
        robot.turn_left();
        assert_eq!(robot.dir, (-1, 0));
        robot.turn_left();
        assert_eq!(robot.dir, (0, -1));
        robot.turn_left();
        assert_eq!(robot.dir, (1, 0));
        robot.turn_left();
        assert_eq!(robot.dir, (0, 1));
    }
}
