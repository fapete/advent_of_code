use std::collections::HashMap;

use day25::aoclib::*;
//use num_bigint::{BigUint, ToBigUint};

fn main() {
    println!("Encryption Key: {}", solve("input"));
}

fn transform_subject_no(subject: u128, loop_size: u128) -> u128 {
    let mut value: u128 = 1;
    for _ in 0..loop_size {
        value = value * subject;
        value = value % 20201227;
    }
    value
    //subject.pow(loop_size) % 20201227 // Overflows on test input, so better do it step by step
}

// Brute force takes too long
fn brute_force_attack(target_key: u128) -> u128 {
    let mut loop_size = 1;
    while transform_subject_no(7, loop_size) != target_key {
        loop_size += 1;
    }
    loop_size
}

fn baby_step_giant_step(g: u128, target: u128) -> Option<u128> {
    let m = (20201227f64).sqrt().ceil() as u128;
    let mut table = HashMap::new();
    let mut e = 1;
    for i in 0..m {
        table.insert(e, i);
        e = (e * g) % 20201227;
    }
    let factor = pow_m(g, (20201227 - m - 1) as i64, 20201227);
    e = target;
    for i in 0..m {
        if let Some((k, v)) = table.get_key_value(&e) {
            return Some(i * m + v);
        }
        e = (e * factor) % 20201227;
    }
    None
}

fn pow_m(mut base: u128, mut exp: i64, m: u128) -> u128 {
    if exp == 0 {
        return 1;
    }
    let mut y = 1;
    while exp > 1 {
        if exp % 2 == 0 {
            base = (base * base) % m;
            exp /= 2;
        } else {
            y = (y * base) % m;
            base = (base * base) % m;
            exp = (exp - 1) / 2;
        }
    }
    base * y
}

fn solve(fname: &str) -> u128 {
    let input = read_file(fname).unwrap();
    let mut input = input.lines();
    let card_key = input.next().unwrap().parse::<u128>().unwrap();
    let door_key = input.next().unwrap().parse::<u128>().unwrap();

    if let Some(card_loop) = baby_step_giant_step(7, card_key) {
        let encryption_key = transform_subject_no(door_key, card_loop);
        encryption_key
    } else {
        0
    }
    //let door_loop = brute_force_attack(door_key);

    //assert_eq!(encryption_key, transform_subject_no(card_key, door_loop));
}

#[test]
fn test_transform_subject_no() {
    assert_eq!(transform_subject_no(7, 8), 5764801);
    assert_eq!(transform_subject_no(7, 11), 17807724);
    assert_eq!(transform_subject_no(17807724, 8), 14897079);
    assert_eq!(transform_subject_no(5764801, 11), 14897079);
}

#[test]
fn test_brute_force_attack() {
    assert_eq!(brute_force_attack(5764801), 8);
    assert_eq!(brute_force_attack(17807724), 11);
}

#[test]
fn test_baby_steps() {
    assert_eq!(baby_step_giant_step(7, 5764801), Some(8));
    assert_eq!(baby_step_giant_step(7, 17807724), Some(11));
}

#[test]
fn test_solve() {
    assert_eq!(solve("test"), 14897079);
}
