use day18::aoclib::*;

fn main() {
    println!("The sum of every Expression is: {}", solve1("input"));
    println!(
        "The sum of every Expression in part 2 is: {}",
        solve2("input")
    );
}

#[derive(Debug, PartialEq)]
enum Token {
    Num(i64),
    Add,
    Mult,
    Open,
    Close,
}

fn add(x: i64, y: i64) -> i64 {
    x + y
}

fn mult(x: i64, y: i64) -> i64 {
    x * y
}

fn eval2<'a, T>(tokens: &mut T) -> i64
where
    T: Iterator<Item = &'a Token>,
{
    let mut number_stack: Vec<i64> = Vec::new();
    let mut operator_stack = Vec::new();
    while let Some(t) = tokens.next() {
        match t {
            Token::Num(x) => number_stack.push(*x),
            Token::Add => operator_stack.push(Token::Add),
            Token::Mult => {
                while let Some(Token::Add) = operator_stack.last() {
                    operator_stack.pop();
                    pop_number_stack_with(&mut number_stack, add);
                }
                operator_stack.push(Token::Mult);
            }
            Token::Open => operator_stack.push(Token::Open),
            Token::Close => {
                while let Some(t) = operator_stack.pop() {
                    match t {
                        Token::Open => break,
                        Token::Add => pop_number_stack_with(&mut number_stack, add),
                        Token::Mult => pop_number_stack_with(&mut number_stack, mult),
                        _ => panic!("Invalid Operator during Parsing"),
                    }
                }
            }
        }
    }

    while let Some(t) = operator_stack.pop() {
        match t {
            Token::Add => pop_number_stack_with(&mut number_stack, add),
            Token::Mult => pop_number_stack_with(&mut number_stack, mult),
            _ => panic!("Invalid operator at this point!"),
        }
    }
    number_stack.pop().unwrap()
}

fn pop_number_stack_with(numbers: &mut Vec<i64>, operation: fn(i64, i64) -> i64) {
    let n1 = numbers.pop().unwrap();
    let n2 = numbers.pop().unwrap();
    numbers.push(operation(n1, n2));
}

fn eval1<'a, T>(tokens: &mut T) -> i64
where
    T: Iterator<Item = &'a Token>,
{
    let mut acc = 0;
    let mut operator_fn: fn(i64, i64) -> i64 = add;
    while let Some(t) = tokens.next() {
        match t {
            Token::Num(x) => acc = operator_fn(acc, *x),
            Token::Add => operator_fn = add,
            Token::Mult => operator_fn = mult,
            Token::Open => acc = operator_fn(acc, eval1(tokens)),
            Token::Close => return acc,
        }
    }
    acc
}

fn tokenize(expression: &str) -> Vec<Token> {
    let mut result = Vec::new();
    for x in expression.split(" ") {
        // Consecutive opening/closing parentheses are not separated by whitespace
        if x.starts_with("(") {
            let opening = x.chars().take_while(|c| *c == '(').count();
            for _ in 0..opening {
                result.push(Token::Open);
            }
            result.push(Token::Num(x[opening..].parse::<i64>().unwrap()));
        } else if x.ends_with(")") {
            let closing = x.chars().rev().take_while(|c| *c == ')').count();

            result.push(Token::Num(
                x[0..(x.len() - closing)].parse::<i64>().unwrap(),
            ));

            for _ in 0..closing {
                result.push(Token::Close);
            }
        } else if x == "+" {
            result.push(Token::Add);
        } else if x == "*" {
            result.push(Token::Mult);
        } else {
            result.push(Token::Num(x.parse::<i64>().unwrap()))
        }
    }
    result
}

fn solve1(fname: &str) -> i64 {
    let homework = read_file(fname).unwrap();
    homework
        .lines()
        .map(|exp| eval1(&mut tokenize(exp).iter()))
        .sum()
}

fn solve2(fname: &str) -> i64 {
    let homework = read_file(fname).unwrap();
    homework
        .lines()
        .map(|exp| eval2(&mut tokenize(exp).iter()))
        .sum()
}

#[test]
fn test_tokenize() {
    assert_eq!(
        tokenize("1 + 4 * 45"),
        vec![
            Token::Num(1),
            Token::Add,
            Token::Num(4),
            Token::Mult,
            Token::Num(45),
        ],
    );

    assert_eq!(
        tokenize("1 + (4 * 45)"),
        vec![
            Token::Num(1),
            Token::Add,
            Token::Open,
            Token::Num(4),
            Token::Mult,
            Token::Num(45),
            Token::Close,
        ],
    );

    assert_eq!(
        tokenize("1 + ((4 * 45) + (13 * 7))"),
        vec![
            Token::Num(1),
            Token::Add,
            Token::Open,
            Token::Open,
            Token::Num(4),
            Token::Mult,
            Token::Num(45),
            Token::Close,
            Token::Add,
            Token::Open,
            Token::Num(13),
            Token::Mult,
            Token::Num(7),
            Token::Close,
            Token::Close,
        ],
    );
}

#[test]
fn test_eval1() {
    assert_eq!(eval1(&mut tokenize("1 + 4 * 45").iter()), 225);
    assert_eq!(eval1(&mut tokenize("1 + (4 * 50)").iter()), 201);
    assert_eq!(
        eval1(&mut tokenize("1 + ((4 * 50) + (13 * 7))").iter()),
        292
    );
    assert_eq!(
        eval1(&mut tokenize("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)").iter()),
        13632
    );
}

#[test]
fn test_eval2() {
    assert_eq!(eval2(&mut tokenize("1 + 4 * 45").iter()), 225);
    assert_eq!(eval2(&mut tokenize("2 + 2 * 3 + 2").iter()), 20);
    assert_eq!(eval2(&mut tokenize("2 + (2 * 3) + 2").iter()), 10);
    assert_eq!(
        eval2(&mut tokenize("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)").iter()),
        23340
    );
}
