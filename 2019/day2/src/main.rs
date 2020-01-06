struct Machine {
    tape: Vec<usize>,
    pos: usize,
    state: MachineState,
}

enum MachineState {
    Cont,
    Halt,
    Error,
}

impl Machine {
    fn new(tape: Vec<usize>) -> Machine {
        Machine {
            tape,
            pos: 0,
            state: MachineState::Cont,
        }
    }

    fn exec_instruction_and_move(&mut self) {
        match self.tape[self.pos] {
            1 => {
                let par1 = self.tape[self.pos + 1];
                let par2 = self.tape[self.pos + 2];
                let par3 = self.tape[self.pos + 3];
                self.tape[par3] = self.tape[par1] + self.tape[par2];
                self.pos += 4;
            }
            2 => {
                let par1 = self.tape[self.pos + 1];
                let par2 = self.tape[self.pos + 2];
                let par3 = self.tape[self.pos + 3];
                self.tape[par3] = self.tape[par1] * self.tape[par2];
                self.pos += 4;
            }
            99 => self.state = MachineState::Halt,
            _ => self.state = MachineState::Error,
        }
    }

    fn run(&mut self, par1: usize, par2: usize) -> usize {
        self.tape[1] = par1;
        self.tape[2] = par2;

        loop {
            match self.state {
                MachineState::Cont => self.exec_instruction_and_move(),
                MachineState::Halt => break self.result(),
                MachineState::Error => panic!("Insert Error Handling here"),
            }
        }
    }

    fn result(&self) -> usize {
        self.tape[0]
    }

    fn replace_tape(&mut self, tape: Vec<usize>) {
        self.tape = tape;
        self.pos = 0;
        self.state = MachineState::Cont;
    }
}

fn main() {
    let init_tape = read_tape();
    let mut computer = Machine::new(init_tape.clone());

    // Part 1: Exec with 12 and 2
    let result = computer.run(12, 2);
    println!("The result is {}", result);

    computer.replace_tape(init_tape.clone());

    let desired_output = 19690720;
    let mut noun = 0;
    let mut verb = 0;
    while computer.run(noun, verb) != desired_output {
        verb += 1;
        if verb == 100 {
            noun += 1;
            verb = 0;
        }
        if noun == 100 {
            panic!("No combination worked!");
        }
        computer.replace_tape(init_tape.clone());
    }

    println!(
        "The Part 2 result is: 100 * {} + {} = {}",
        noun,
        verb,
        100 * noun + verb
    );
}

fn read_tape() -> Vec<usize> {
    include_str!("../input")
        .trim()
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect()
}
