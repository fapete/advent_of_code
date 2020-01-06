fn main() {
    let list_of_fuels = fuels();
    println!("Answer to part 1: {}", list_of_fuels.iter().sum::<i32>());
    println!(
        "Answer to part 2: {}",
        list_of_fuels.iter().map(|x| part2(x)).sum::<i32>()
    );
}

fn fuels() -> Vec<i32> {
    let file = include_str!("../input");

    file.lines()
        .map(|x| i32::from_str_radix(x, 10).unwrap())
        .map(|x| x / 3 - 2)
        .collect()
}

fn part2(val: &i32) -> i32 {
    let mut sum = *val;
    let mut to_add = sum;

    loop {
        to_add = to_add / 3 - 2;
        if to_add <= 0 {
            break sum;
        }
        sum += to_add;
    }
}
