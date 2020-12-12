/*
 * Common Lib to extend with functions/structs I expect to be reusing on other days.
 */
pub mod aoclib {
    use std::fs::File;
    use std::io::prelude::*;

    pub fn read_file(fname: &str) -> std::io::Result<String> {
        let mut file = File::open(fname)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        return Ok(content);
    }

    #[derive(Debug)]
    pub struct Computer {
        tape: Vec<TapePosition>,
        pc: isize,
        acc: i64,
        state: State,
    }

    #[derive(Debug)]
    struct TapePosition {
        op: Operation,
        arg: i64,
        seen: bool,
    }

    #[derive(Copy, Clone, Debug, PartialEq)]
    enum Operation {
        NOP,
        ACC,
        JMP,
    }

    #[derive(PartialEq, Debug)]
    pub enum State {
        Running,
        InfLoop,
        Term,
    }

    impl Computer {
        pub fn from(listing: String) -> Computer {
            Computer {
                tape: listing.lines().map(TapePosition::from).collect(),
                pc: 0,
                acc: 0,
                state: State::Running,
            }
        }

        pub fn step(&mut self) -> &State {
            if self.pc as usize >= self.tape.len() {
                self.state = State::Term;
            } else {
                let tape_position = &mut self.tape[self.pc as usize];
                if tape_position.seen {
                    self.state = State::InfLoop;
                } else {
                    let op = tape_position.op;
                    let arg = tape_position.arg;

                    match &op {
                        Operation::NOP => {
                            self.pc += 1;
                        }
                        Operation::ACC => {
                            self.pc += 1;
                            self.acc += arg
                        }
                        Operation::JMP => self.pc += arg as isize,
                    };

                    tape_position.seen = true;
                }
            }
            &self.state
        }

        pub fn get_acc(&self) -> i64 {
            self.acc
        }

        pub fn rewind(&mut self, flipped_pos: usize) {
            match self.tape[flipped_pos].op {
                Operation::NOP => self.tape[flipped_pos].op = Operation::JMP,
                Operation::JMP => self.tape[flipped_pos].op = Operation::NOP,
                Operation::ACC => panic!("Cannot flip ACC!"),
            }

            for tape_pos in &mut self.tape {
                tape_pos.seen = false;
            }

            self.pc = 0;
            self.acc = 0;
            self.state = State::Running;
        }

        pub fn flip_next_from(&mut self, mut from: usize) -> usize {
            while self.tape[from].op == Operation::ACC {
                from += 1;
            }

            match self.tape[from].op {
                Operation::NOP => self.tape[from].op = Operation::JMP,
                Operation::JMP => self.tape[from].op = Operation::NOP,
                Operation::ACC => panic!("Cannot flip ACC!"),
            }

            from
        }

        pub fn has_terminated(&self) -> bool {
            self.state == State::Term
        }
    }

    impl TapePosition {
        fn from(line: &str) -> TapePosition {
            let elements: Vec<_> = line.split(" ").collect();
            let op = match elements[0] {
                "nop" => Operation::NOP,
                "acc" => Operation::ACC,
                "jmp" => Operation::JMP,
                _ => panic!("Unknown opcode!"),
            };
            let arg = elements[1].parse::<i64>().unwrap();
            TapePosition {
                op,
                arg,
                seen: false,
            }
        }
    }
}
