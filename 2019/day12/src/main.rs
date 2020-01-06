use std::cell::RefCell;
use std::cmp::Ordering;

#[derive(Debug)]
struct Moon {
    pos: (i32, i32, i32),
    vel: RefCell<(i32, i32, i32)>,
}

impl Moon {
    fn new(x: i32, y: i32, z: i32) -> Moon {
        Moon {
            pos: (x, y, z),
            vel: RefCell::new((0, 0, 0)),
        }
    }

    fn adjust_velocity_towards(&self, other: &Self) {
        let dx = compare_and_adjust(self.pos.0, other.pos.0, self.vel.borrow().0);
        let dy = compare_and_adjust(self.pos.1, other.pos.1, self.vel.borrow().1);
        let dz = compare_and_adjust(self.pos.2, other.pos.2, self.vel.borrow().2);
        *(self.vel.borrow_mut()) = (dx, dy, dz);
    }

    fn mv(&mut self) {
        let dx = self.vel.borrow().0;
        let dy = self.vel.borrow().1;
        let dz = self.vel.borrow().2;
        let (x, y, z) = self.pos;
        self.pos = (x + dx, y + dy, z + dz);
    }

    fn energy(&self) -> i32 {
        let (x, y, z) = self.pos;
        let dx = self.vel.borrow().0;
        let dy = self.vel.borrow().1;
        let dz = self.vel.borrow().2;
        let pot = x.abs() + y.abs() + z.abs();
        let kin = dx.abs() + dy.abs() + dz.abs();
        pot * kin
    }
}

fn compare_and_adjust(x1: i32, x2: i32, dx1: i32) -> i32 {
    match x1.cmp(&x2) {
        Ordering::Greater => dx1 - 1,
        Ordering::Less => dx1 + 1,
        Ordering::Equal => dx1,
    }
}

fn parse_positions(tuple: &str) -> (i32, i32, i32) {
    let positions: Vec<i32> = tuple[1..tuple.len() - 1]
        .split(',')
        .map(|x| {
            x.split('=')
                .skip(1)
                .map(|y| y.parse::<i32>().unwrap())
                .next()
                .unwrap()
        })
        .collect();
    (positions[0], positions[1], positions[2])
}

fn main() {
    let mut moons = Vec::new();
    let file = include_str!("../input");
    for line in file.lines() {
        let (x, y, z) = parse_positions(line);
        moons.push(Moon::new(x, y, z));
    }

    // part 1:
    for _ in 0..1000 {
        for moon in &moons {
            for moon2 in &moons {
                moon.adjust_velocity_towards(&moon2);
            }
        }
        for moon in &mut moons {
            moon.mv();
        }
    }

    println!(
        "The energy in the system is: {}",
        moons.iter().map(|x| x.energy()).sum::<i32>()
    );

    // part 2:
    let mut first_x_stop = 0;
    let mut first_y_stop = 0;
    let mut first_z_stop = 0;
    let mut step = 0;

    let mut moons = Vec::new();
    for line in file.lines() {
        let (x, y, z) = parse_positions(line);
        moons.push(Moon::new(x, y, z));
    }

    loop {
        step += 1;
        for moon in &moons {
            for moon2 in &moons {
                moon.adjust_velocity_towards(&moon2);
            }
        }

        if first_x_stop == 0 && moons.iter().all(|x| x.vel.borrow().0 == 0) {
            first_x_stop = step;
        }
        if first_y_stop == 0 && moons.iter().all(|x| x.vel.borrow().1 == 0) {
            first_y_stop = step;
        }
        if first_z_stop == 0 && moons.iter().all(|x| x.vel.borrow().2 == 0) {
            first_z_stop = step;
        }

        for moon in &mut moons {
            moon.mv();
        }
        if first_x_stop > 0 && first_y_stop > 0 && first_z_stop > 0 {
            break;
        }
    }

    println!(
        "First changes in direction: {}, {}, {}",
        first_x_stop, first_y_stop, first_z_stop
    );
    // From here use external tool to find least common multiplier of these three numbers and
    // double that to get result.
}
