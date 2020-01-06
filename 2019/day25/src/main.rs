use day25::intcode_computer::Machine;
use std::io;

struct ASCII {
    computer: Machine,
}

impl ASCII {
    fn new(mut computer: Machine) -> ASCII {
        computer.run();
        ASCII { computer }
    }

    fn show_output_and_handle_input(&mut self) {
        while let Some(i) = self.computer.pop_output_front() {
            if i < 256 {
                print!("{}", char::from(i as u8));
            } else {
                println!("Output: {}", i);
            }
        }
        if self.computer.is_waiting() {
            let mut input = String::new();

            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            self.computer.set_input(
                format!("{}", input)
                    .chars()
                    .map(|x| (x as u8) as isize)
                    .rev()
                    .collect(),
            );
        }
        self.computer.cont();
    }
}

fn main() {
    // part 1
    let mut camera = ASCII::new(get_computer());

    loop {
        camera.show_output_and_handle_input();
    }
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
