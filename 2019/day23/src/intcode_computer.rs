use std::collections::VecDeque;
use std::fmt;
use std::sync::mpsc;
use std::thread;

#[derive(Debug, Clone, Copy)]
pub struct Packet {
    from: usize,
    to: usize,
    x: isize,
    y: isize,
}

impl Packet {
    pub fn new(from: usize, to: usize, x: isize, y: isize) -> Packet {
        Packet { from, to, x, y }
    }

    pub fn get_receiver(&self) -> usize {
        self.to
    }

    pub fn get_message(self) -> (isize, isize) {
        (self.x, self.y)
    }

    pub fn read_y(&self) -> isize {
        self.y
    }
}

pub struct Machine {
    tape: Vec<isize>,
    pos: usize,
    state: MachineState,
    outputs: mpsc::Sender<Packet>,
    inputs: Option<mpsc::Receiver<Packet>>,
    relative_base: isize,
    init_tape: Vec<isize>,
    input_buffer: VecDeque<isize>,
    output_buffer: VecDeque<isize>,
    id: usize,
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
enum MachineState {
    Cont,
    Halt,
    Error,
}

impl fmt::Debug for Machine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Machine {{ state: {:?}, pos: {}, inputs: {:?}, outputs: {:?} }}",
            self.state, self.pos, self.inputs, self.outputs
        )
    }
}

impl Machine {
    pub fn new(tape: Vec<isize>, tx: mpsc::Sender<Packet>, address: isize) -> Machine {
        let mut input_buffer = VecDeque::new();
        input_buffer.push_back(address);
        Machine {
            tape: tape.clone(),
            pos: 0,
            state: MachineState::Cont,
            outputs: tx,
            inputs: None,
            relative_base: 0,
            init_tape: tape,
            input_buffer,
            output_buffer: VecDeque::new(),
            id: address as usize,
        }
    }

    pub fn from(tape: &str, tx: mpsc::Sender<Packet>, address: isize) -> Machine {
        Machine::new(
            tape.trim()
                .split(',')
                .map(|x| x.parse::<isize>().unwrap())
                .collect(),
            tx,
            address,
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
                self.input(par1);
                self.pos += 2;
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

    fn input(&mut self, par1: usize) -> () {
        if self.input_buffer.len() == 0 {
            match &self.inputs {
                None => panic!("Machine is not running!"),
                Some(rx) => match rx.try_recv() {
                    Ok(i) => {
                        let (x, y) = i.get_message();
                        self.tape[par1] = x;
                        self.input_buffer.push_back(y);
                    }
                    Err(_) => {
                        self.tape[par1] = -1;
                    }
                },
            }
        } else {
            let val = self.input_buffer.pop_front().unwrap();
            self.tape[par1] = val;
        }
    }

    fn output(&mut self, par1: isize) {
        if self.output_buffer.len() < 2 {
            self.output_buffer.push_back(par1);
        } else {
            let to = self.output_buffer.pop_front().unwrap();
            let x = self.output_buffer.pop_front().unwrap();
            self.outputs
                .send(Packet::new(self.id, to as usize, x, par1))
                .unwrap();
        }
    }

    fn add(&mut self, par1: isize, par2: isize) -> isize {
        par1 + par2
    }

    fn mul(&mut self, par1: isize, par2: isize) -> isize {
        par1 * par2
    }

    pub fn run(mut self) -> (thread::JoinHandle<()>, mpsc::Sender<Packet>) {
        let (tx, rx) = mpsc::channel();
        self.inputs = Some(rx);
        (
            thread::spawn(move || loop {
                match self.state {
                    MachineState::Cont => self.exec_instruction_and_move(),
                    MachineState::Halt => break, // self.result(),
                    MachineState::Error => panic!("Insert Error Handling here"),
                }
            }),
            tx,
        )
    }

    pub fn has_halted(&self) -> bool {
        self.state == MachineState::Halt
    }

    pub fn set_tape_pos(&mut self, pos: usize, val: isize) {
        self.tape[pos] = val;
    }

    pub fn replace_tape(&mut self, tape: Vec<isize>) {
        self.tape = tape;
        self.pos = 0;
        self.state = MachineState::Cont;
        self.relative_base = 0;
    }

    pub fn rewind(&mut self) {
        self.tape = self.init_tape.clone();
        self.pos = 0;
        self.state = MachineState::Cont;
        self.relative_base = 0;
    }
}
