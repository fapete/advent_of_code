use day7::intcode_computer::Machine;

struct Amplifier {
    phase: u32,
    computer: Machine,
    output: isize,
}

impl Amplifier {
    fn new(computer_tape: &str, phase: u32) -> Amplifier {
        Amplifier {
            phase,
            computer: Machine::from(computer_tape),
            output: 0,
        }
    }

    fn run_computer(&mut self, prev_out: isize) {
        if self.computer.is_waiting() {
            self.computer.add_input(prev_out);
            self.computer.cont();
        } else {
            self.computer
                .set_input(vec![prev_out as isize, self.phase as isize]);
            self.computer.run()
        }
        self.output = *self.computer.get_output().last().unwrap();
    }
}

fn main() {
    part1();
    part2();
}

fn part1() {
    let mut output_signals = Vec::new();
    for permutation in get_permutations(0, 5) {
        let mut amplifier_config = get_amplifiers(permutation);
        amplifier_config[0].run_computer(0);
        for i in 1..5 {
            amplifier_config[i].run_computer(amplifier_config[i - 1].output);
        }
        output_signals.push((amplifier_config[4].output, permutation));
    }

    let max = output_signals.iter().max_by(|x, y| x.0.cmp(&y.0)).unwrap();
    println!("The maximum signal is: {} (phases: {:?})", max.0, max.1);
}

fn part2() {
    let mut output_signals = Vec::new();
    for permutation in get_permutations(5, 10) {
        let mut amplifier_config = get_amplifiers(permutation);
        amplifier_config[0].run_computer(0);
        let mut cur_amp = 1;
        while !amplifier_config[4].computer.has_halted() {
            amplifier_config[cur_amp % 5].run_computer(amplifier_config[(cur_amp - 1) % 5].output);
            cur_amp += 1;
        }
        output_signals.push((amplifier_config[4].output, permutation));
    }

    let max = output_signals.iter().max_by(|x, y| x.0.cmp(&y.0)).unwrap();
    println!("The maximum signal is: {} (phases: {:?})", max.0, max.1);
}

fn get_permutations(from: u32, to: u32) -> Vec<[u32; 5]> {
    let mut permutations = Vec::new();
    for i1 in from..to {
        for i2 in (from..to).filter(|x| *x != i1) {
            for i3 in (from..to).filter(|x| *x != i2 && *x != i1) {
                for i4 in (from..to).filter(|x| *x != i3 && *x != i1 && *x != i2) {
                    for i5 in (from..to).filter(|x| *x != i4 && *x != i1 && *x != i2 && *x != i3) {
                        permutations.push([i1, i2, i3, i4, i5]);
                    }
                }
            }
        }
    }
    permutations
}

fn get_amplifiers(phases: [u32; 5]) -> [Amplifier; 5] {
    let tape = include_str!("../input");
    [
        Amplifier::new(tape, phases[0]),
        Amplifier::new(tape, phases[1]),
        Amplifier::new(tape, phases[2]),
        Amplifier::new(tape, phases[3]),
        Amplifier::new(tape, phases[4]),
    ]
}
