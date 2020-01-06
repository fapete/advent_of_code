use day9::intcode_computer::Machine;

fn main() {
    part1();
    part2();
}

fn part1() {
    let mut computer = get_computer();
    computer.add_input(1);
    computer.run();
    println!("Output: {:?}", computer.get_output());
}

fn part2() {
    let mut computer = get_computer();
    computer.add_input(2);
    computer.run();
    println!("Output: {:?}", computer.get_output());
}

fn get_computer() -> Machine {
    let tape = include_str!("../input");
    Machine::from(tape)
}
