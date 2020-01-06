use day21::intcode_computer::Machine;

struct ASCII {
    computer: Machine,
}

impl ASCII {
    fn new(mut computer: Machine) -> ASCII {
        computer.run();
        ASCII { computer }
    }

    fn print_view(&mut self) {
        while let Some(i) = self.computer.pop_output_front() {
            if i < 256 {
                print!("{}", char::from(i as u8));
            } else {
                println!("Output: {}", i);
            }
        }
        self.computer.cont();
    }

    fn program_robot_part1(&mut self) {
        // Idea: Jump if at least one of A,B,C is hole and D is ground.
        let main = "NOT A T\nNOT B J\nOR J T\nNOT C J\nOR J T\nNOT D J\nNOT J J\nAND T J\nWALK\n";

        self.computer.set_input(
            format!("{}", main)
                .chars()
                .map(|x| (x as u8) as isize)
                .rev()
                .collect(),
        );
        self.computer.cont();
    }

    fn program_robot_part2(&mut self) {
        // Idea: Jump if at least one of A,B,C is hole and D is ground.
        let main = "NOT A T\nNOT B J\nOR J T\nNOT C J\nOR J T\nNOT D J\nNOT J J\nAND T J\nAND E T\nOR H T\nAND T J\nRUN\n";

        self.computer.set_input(
            format!("{}", main)
                .chars()
                .map(|x| (x as u8) as isize)
                .rev()
                .collect(),
        );
        self.computer.cont();
    }
}

fn main() {
    // part 1
    let mut camera = ASCII::new(get_computer());
    camera.print_view();
    camera.program_robot_part1();
    camera.print_view();

    // part 2
    camera.computer.rewind();
    camera.print_view();
    camera.program_robot_part2();
    camera.print_view();
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
