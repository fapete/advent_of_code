use day19::intcode_computer::Machine;
use std::collections::HashSet;

type Pos = (u64, u64);

struct TractorBeam {
    control: Machine,
    affected_positions: HashSet<Pos>,
}

impl TractorBeam {
    fn new(control: Machine) -> TractorBeam {
        TractorBeam {
            control,
            affected_positions: HashSet::new(),
        }
    }

    fn test_position(&mut self, (x, y): Pos) -> bool {
        self.affected_positions.contains(&(x, y)) || {
            self.control.run();
            if self.control.is_waiting() {
                self.control.add_input(x as isize);
                self.control.cont();
            }
            if self.control.is_waiting() {
                self.control.add_input(y as isize);
                self.control.cont();
            }
            while !self.control.has_halted() {}
            let out = self.control.pop_output().unwrap();
            self.control.rewind();
            if out == 1 {
                self.affected_positions.insert((x, y));
            }
            out == 1
        }
    }
}

fn deploy_drones(beam: &mut TractorBeam) {
    for x in 0..50 {
        for y in 0..50 {
            beam.test_position((x, y));
        }
    }
}

fn part2(b: &mut TractorBeam) -> Pos {
    for y in 100..1500 {
        for x in 100..1500 {
            b.test_position((x, y));
        }
        if let Some((x2, y2)) = b
            .affected_positions
            .iter()
            .filter(|(_, z)| *z == y)
            .max_by(|p, q| p.0.cmp(&q.0))
            .cloned()
        {
            if b.test_position((x2 - 99, y2 + 99)) {
                return (x2 - 99, y2);
            }
        }
    }
    (0, 0)
}

fn main() {
    let mut beam = TractorBeam::new(get_computer());
    deploy_drones(&mut beam);
    println!("Affected positions: {}", beam.affected_positions.len());
    let pos = part2(&mut beam);
    println!("found point: {:?}", pos);
    println!("Makes answer: {}", pos.0 * 10000 + pos.1);
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
