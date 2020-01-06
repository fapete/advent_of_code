pub struct Machine {
    tape: Vec<isize>,
    pos: usize,
    state: MachineState,
    outputs: Vec<isize>,
    inputs: Vec<isize>,
    relative_base: isize,
}

#[derive(Eq, PartialEq)]
enum MachineState {
    Cont,
    Halt,
    Error,
    Waiting,
}

impl Machine {
    pub fn new(tape: Vec<isize>) -> Machine {
        Machine {
            tape,
            pos: 0,
            state: MachineState::Cont,
            outputs: Vec::new(),
            inputs: Vec::new(),
            relative_base: 0,
        }
    }

    pub fn from(tape: &str) -> Machine {
        Machine::new(
            tape.trim()
                .split(',')
                .map(|x| x.parse::<isize>().unwrap())
                .collect(),
        )
    }

    fn exec_instruction_and_move(&mut self) {
        let opcode = self.tape[self.pos] % 100;
        // Compute modes/parameters. We compute the maximum amount, even if operation takes less,
        // the superfluos ones are just 0 in that case.
        let mode_par1 = ((self.tape[self.pos] / 100) % 10) as usize;
        let mode_par2 = ((self.tape[self.pos] / 1000) % 10) as usize;
        let mode_par3 = ((self.tape[self.pos] / 10000) % 10) as usize;
        let par1 = if self.tape.len() > self.pos + 1 {
            self.tape[self.pos + 1]
        } else {
            0
        };
        let par2 = if self.tape.len() > self.pos + 2 {
            self.tape[self.pos + 2]
        } else {
            0
        };
        let par3 = if self.tape.len() > self.pos + 3 {
            self.tape[self.pos + 3]
        } else {
            0
        };
        match opcode {
            1 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                let par3 = self.get_address(par3, mode_par3);
                self.tape[par3] = self.add(par1, par2);
                self.pos += 4;
            }
            2 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                let par3 = self.get_address(par3, mode_par3);
                self.tape[par3] = self.mul(par1, par2);
                self.pos += 4;
            }
            3 => {
                let par1 = self.get_address(par1, mode_par1);
                match self.input(par1) {
                    MachineState::Cont => self.pos += 2,
                    MachineState::Waiting => self.state = MachineState::Waiting,
                    _ => (),
                }
            }
            4 => {
                let par1 = self.get_value(par1, mode_par1);
                self.output(par1);
                self.pos += 2;
            }
            5 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                self.jump_if_true(par1, par2);
            }
            6 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                self.jump_if_false(par1, par2);
            }
            7 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                let par3 = self.get_address(par3, mode_par3);
                self.tape[par3] = if self.lt(par1, par2) { 1 } else { 0 };
                self.pos += 4;
            }
            8 => {
                let par1 = self.get_value(par1, mode_par1);
                let par2 = self.get_value(par2, mode_par2);
                let par3 = self.get_address(par3, mode_par3);
                self.tape[par3] = if self.eq(par1, par2) { 1 } else { 0 };
                self.pos += 4;
            }
            9 => {
                let par1 = self.get_value(par1, mode_par1);
                self.relative_base += par1;
                self.pos += 2;
            }
            99 => self.state = MachineState::Halt,
            _ => self.state = MachineState::Error,
        }
    }

    fn check_tape_length(&mut self, len: usize) {
        if self.tape.len() <= len {
            self.tape.resize(len + 1, 0);
        }
    }

    fn get_address(&mut self, par: isize, mode: usize) -> usize {
        match mode {
            0 => {
                self.check_tape_length(par as usize);
                par as usize
            }
            2 => {
                self.check_tape_length((par + self.relative_base) as usize);
                (par + self.relative_base) as usize
            }
            1 => panic!("Cannot get address of immediate mode parameter"),
            _ => panic!("Cannot get address of unknown mode"),
        }
    }

    fn get_value(&mut self, par: isize, mode: usize) -> isize {
        match mode {
            0 => {
                self.check_tape_length(par as usize);
                self.tape[par as usize]
            }
            1 => par,
            2 => {
                self.check_tape_length((par + self.relative_base) as usize);
                self.tape[(par + self.relative_base) as usize]
            }
            _ => {
                panic!("Unsupported mode given!");
            }
        }
    }

    fn jump_if_true(&mut self, par1: isize, par2: isize) {
        if par1 != 0 {
            self.pos = par2 as usize;
        } else {
            self.pos += 3;
        }
    }

    fn jump_if_false(&mut self, par1: isize, par2: isize) {
        if par1 == 0 {
            self.pos = par2 as usize;
        } else {
            self.pos += 3;
        }
    }

    fn lt(&self, par1: isize, par2: isize) -> bool {
        par1 < par2
    }

    fn eq(&self, par1: isize, par2: isize) -> bool {
        par1 == par2
    }

    fn input(&mut self, par1: usize) -> MachineState {
        match self.inputs.pop() {
            None => MachineState::Waiting,
            Some(i) => {
                self.tape[par1] = i;
                MachineState::Cont
            }
        }
    }

    fn output(&mut self, par1: isize) {
        self.outputs.push(par1);
    }

    fn add(&mut self, par1: isize, par2: isize) -> isize {
        par1 + par2
    }

    fn mul(&mut self, par1: isize, par2: isize) -> isize {
        par1 * par2
    }

    pub fn run(&mut self) -> () {
        loop {
            match self.state {
                MachineState::Cont => self.exec_instruction_and_move(),
                MachineState::Halt => break, // self.result(),
                MachineState::Waiting => break,
                MachineState::Error => panic!("Insert Error Handling here"),
            }
        }
    }

    pub fn cont(&mut self) {
        self.state = MachineState::Cont;
        self.run();
    }

    pub fn set_input(&mut self, inputs: Vec<isize>) {
        self.inputs = inputs;
    }

    pub fn add_input(&mut self, input: isize) {
        self.inputs.push(input);
    }

    pub fn get_output(&self) -> &Vec<isize> {
        &self.outputs
    }

    pub fn has_halted(&self) -> bool {
        self.state == MachineState::Halt
    }

    pub fn is_waiting(&self) -> bool {
        self.state == MachineState::Waiting
    }

    pub fn replace_tape(&mut self, tape: Vec<isize>) {
        self.tape = tape;
        self.pos = 0;
        self.state = MachineState::Cont;
        self.outputs = Vec::new();
        self.relative_base = 0;
    }
}
