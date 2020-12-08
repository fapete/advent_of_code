use day8::aoclib::*;

fn main() {
    println!("Accumulator at first repeat: {}", solve1("input"));
    println!("Accumulator once program terminates: {}", solve2("input"));
}

fn solve1(in_file: &str) -> i64 {
    let mut computer = Computer::from(read_file(in_file).unwrap());
    while let State::Running = computer.step() {}
    computer.get_acc()
}

fn solve2(in_file: &str) -> i64 {
    let mut computer = Computer::from(read_file(in_file).unwrap());
    let mut flip_pos = 0;
    loop {
        flip_pos = computer.flip_next_from(flip_pos) + 1;
        //eprintln!("Flipped at: {}", flip_pos - 1);
        //eprintln!("Computer: {:?}", computer);

        while let State::Running = computer.step() {}

        //eprintln!("Computer: {:?}", computer);

        if computer.has_terminated() {
            break computer.get_acc();
        } else {
            computer.rewind(flip_pos - 1)
        }
    }
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 5)
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test"), 8)
}
