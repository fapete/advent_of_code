fn main() {
    println!(
        "Number of valid Passwords in part 1: {}",
        (372037..905158)
            .map(is_valid_pw_part1)
            .filter(|x| *x)
            .count(),
    );

    println!(
        "Number of valid Passwords in part 2: {}",
        (372037..905158)
            .map(is_valid_pw_part2)
            .filter(|x| *x)
            .count(),
    );
}

fn is_valid_pw_part1(mut num: u32) -> bool {
    let mut decreasing = true;
    let mut double_num = false;
    let mut prev_digit = num % 10;
    while num >= 10 {
        num /= 10;
        let digit = num % 10;
        double_num = digit == prev_digit || double_num;
        decreasing = digit <= prev_digit && decreasing;
        prev_digit = digit;
    }
    double_num && decreasing
}

fn is_valid_pw_part2(mut num: u32) -> bool {
    let mut decreasing = true;
    let mut double_num = false;
    let mut conseq_block = 1;
    let mut prev_digit = num % 10;
    while num >= 10 {
        num /= 10;
        let digit = num % 10;
        if digit == prev_digit {
            conseq_block += 1
        } else {
            double_num = conseq_block == 2 || double_num;
            conseq_block = 1;
        }
        decreasing = digit <= prev_digit && decreasing;
        prev_digit = digit;
    }
    (double_num || conseq_block == 2) && decreasing
}

#[test]
fn test_is_valid_pw_part1() {
    assert!(is_valid_pw_part1(111111));
    assert!(!is_valid_pw_part1(223450));
    assert!(is_valid_pw_part1(223456));
    assert!(!is_valid_pw_part1(123789));
    assert!(is_valid_pw_part1(123778));
}

#[test]
fn test_is_valid_pw_part2() {
    assert!(is_valid_pw_part2(112233));
    assert!(!is_valid_pw_part2(123444));
    assert!(is_valid_pw_part2(123445));
    assert!(!is_valid_pw_part2(111123));
    assert!(is_valid_pw_part2(111122));
}
