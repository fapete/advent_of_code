use day12::aoclib::*;

fn main() {
    println!(
        "Manhattan Distance after following course: {}",
        solve1("input")
    );
    println!(
        "Manhattan Distance after following course using the correct instructions: {}",
        solve2("input")
    );
}

enum Instruction {
    North(i64),
    East(i64),
    Forward(i64),
    Turn(i64),
}

impl Instruction {
    fn from(inst: &str) -> Instruction {
        match &inst[..1] {
            "N" => Instruction::North(inst[1..].parse::<i64>().unwrap()),
            "S" => Instruction::North(-1 * inst[1..].parse::<i64>().unwrap()),
            "E" => Instruction::East(inst[1..].parse::<i64>().unwrap()),
            "W" => Instruction::East(-1 * inst[1..].parse::<i64>().unwrap()),
            "F" => Instruction::Forward(inst[1..].parse::<i64>().unwrap()),
            "R" => Instruction::Turn(360 - inst[1..].parse::<i64>().unwrap()),
            "L" => Instruction::Turn(inst[1..].parse::<i64>().unwrap()),
            _ => panic!("Invalid instruction input!"),
        }
    }
}

struct Ship {
    pos: (i64, i64),
    dir: i64,
    waypoint: (i64, i64),
}

impl Ship {
    fn new() -> Ship {
        Ship {
            pos: (0, 0),
            dir: 0,
            waypoint: (10, 1),
        }
    }

    fn apply1(&mut self, instruction: &Instruction) {
        let (x, y) = self.pos;
        match instruction {
            Instruction::Forward(units) => self.forward(units),
            Instruction::North(units) => self.pos = (x, y + units),
            Instruction::East(units) => self.pos = (x + units, y),
            Instruction::Turn(by) => self.dir = (self.dir + by) % 360,
        }
    }

    fn apply2(&mut self, instruction: &Instruction) {
        let (dx, dy) = self.waypoint;
        match instruction {
            Instruction::Forward(units) => self.forward_waypoint(units),
            Instruction::North(units) => self.waypoint = (dx, dy + units),
            Instruction::East(units) => self.waypoint = (dx + units, dy),
            Instruction::Turn(by) => self.rotate_waypoint(*by),
        }
    }

    fn forward(&mut self, units: &i64) {
        if self.dir == 0 {
            // East
            self.apply1(&Instruction::East(*units));
        } else if self.dir == 90 {
            // North
            self.apply1(&Instruction::North(*units));
        } else if self.dir == 180 {
            // West
            self.apply1(&Instruction::East(-units));
        } else if self.dir == 270 {
            // South
            self.apply1(&Instruction::North(-units));
        } else {
            panic!("Direction not pointing in cardinal directions!");
        }
    }

    fn rotate_waypoint(&mut self, by: i64) {
        let (dx, dy) = self.waypoint;
        if by == 0 {
            // Do nothing, no rotation
        } else if by == 90 {
            // North
            self.waypoint = (-dy, dx);
        } else if by == 180 {
            // West
            self.waypoint = (-dx, -dy);
        } else if by == 270 {
            // South
            self.waypoint = (dy, -dx);
        } else {
            panic!("Direction not pointing in cardinal directions!");
        }
    }

    fn forward_waypoint(&mut self, units: &i64) {
        self.apply1(&Instruction::East(*units * self.waypoint.0));
        self.apply1(&Instruction::North(*units * self.waypoint.1));
    }

    fn manhattan_distance_from_origin(&self) -> i64 {
        let (x, y) = self.pos;
        x.abs() + y.abs()
    }
}

fn get_course(infile: &str) -> Vec<Instruction> {
    read_file(infile)
        .unwrap()
        .lines()
        .map(Instruction::from)
        .collect()
}

fn solve1(infile: &str) -> i64 {
    let course = get_course(infile);

    let mut ship = Ship::new();
    for inst in course.iter() {
        ship.apply1(inst);
    }

    ship.manhattan_distance_from_origin()
}

fn solve2(infile: &str) -> i64 {
    let course = get_course(infile);

    let mut ship = Ship::new();
    for inst in course.iter() {
        ship.apply2(inst);
    }

    ship.manhattan_distance_from_origin()
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 25);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test"), 286);
}
