use std::collections::HashMap;

use day14::aoclib::*;

fn main() {
    let input = read_file("input").unwrap();
    let mut memory = Memory::new();

    println!(
        "Sum of valid memory positions: {}",
        solve1(&mut memory, &input)
    );

    let input = read_file("input").unwrap();
    let mut memory = Memory::new();
    println!(
        "Sum of valid memory positions: {}",
        solve2(&mut memory, &input)
    );
}

struct Memory {
    mem: HashMap<u64, u64>,
    cur_mask_one: u64,
    cur_mask_zero: u64,
    mask_vec: Vec<(u64, u64)>,
}

impl Memory {
    fn new() -> Memory {
        Memory {
            mem: HashMap::new(),
            cur_mask_one: 0,
            cur_mask_zero: 0,
            mask_vec: Vec::new(),
        }
    }

    fn parse_line_part_1(&mut self, line: &str) {
        let parts: Vec<_> = line.split("=").collect();
        if parts[0].trim() == "mask" {
            let (zero_mask, one_mask) = convert_mask_string(parts[1].trim());
            self.cur_mask_one = one_mask;
            self.cur_mask_zero = zero_mask;
        } else {
            let address = parts[0].trim();
            let address = address[4..address.len() - 1].parse::<u64>().unwrap();
            let value = parts[1].trim().parse::<u64>().unwrap();
            self.mem.insert(address, self.apply_mask_to_val(value));
        }
    }

    fn parse_line_part_2(&mut self, line: &str) {
        let parts: Vec<_> = line.split("=").map(|x| x.trim()).collect();
        if parts[0] == "mask" {
            self.mask_vec = all_floating_masks(parts[1]);
        //eprintln!("Masks: {:?}", self.mask_vec);
        } else {
            let address = parts[0][4..parts[0].len() - 1].parse::<u64>().unwrap();
            let value = parts[1].parse::<u64>().unwrap();
            //eprintln!("Add: {}, val: {}", address, value);
            let addresses: Vec<_> = self
                .mask_vec
                .iter()
                .map(|(zero_mask, one_mask)| apply_mask_to_val(*one_mask, *zero_mask, address))
                .collect();
            //eprintln!("Addresses: {:?}", addresses);
            for add in addresses {
                self.mem.insert(add, value);
            }
        }
    }

    fn apply_mask_to_val(&self, val: u64) -> u64 {
        (val | self.cur_mask_one) & self.cur_mask_zero
    }
}

fn apply_mask_to_val(one_mask: u64, zero_mask: u64, val: u64) -> u64 {
    (val | zero_mask) & one_mask
}

fn solve1(memory: &mut Memory, commands: &String) -> u64 {
    for c in commands.lines() {
        memory.parse_line_part_1(c);
    }

    memory.mem.values().sum()
}

fn solve2(memory: &mut Memory, commands: &String) -> u64 {
    for c in commands.lines() {
        memory.parse_line_part_2(c);
    }

    memory.mem.values().sum()
}

fn all_floating_masks(mask: &str) -> Vec<(u64, u64)> {
    let mut result = Vec::new();
    let floating_bits = mask.chars().filter(|x| *x == 'X').count();
    for i in 0..(1 << floating_bits) {
        let mut new_mask_zero = String::from(mask);
        let mut new_mask_one = convert_mask_before_floating(new_mask_zero.as_str());
        let bin_string = format!("{:b}", i);
        //eprintln!("binary: {}", bin_string);
        //eprintln!("Masks: {}, {}", new_mask_zero, new_mask_one);
        for c in bin_string.chars().rev() {
            let last_x_pos = new_mask_zero.rfind('X').unwrap();
            new_mask_zero = format!(
                "{}{}{}",
                &new_mask_zero[0..last_x_pos],
                c,
                &new_mask_zero[last_x_pos + 1..]
            );
            new_mask_one = format!(
                "{}{}{}",
                &new_mask_one[0..last_x_pos],
                c,
                &new_mask_one[last_x_pos + 1..]
            );
            //eprintln!("Masks: {}, {}", new_mask_zero, new_mask_one);
        }
        //eprintln!("Done Converting");
        result.push((
            convert_mask_string_part2(new_mask_zero.as_str()),
            convert_mask_string_part2(new_mask_one.as_str()),
        ));
    }
    result
}

fn convert_mask_before_floating(mask: &str) -> String {
    mask.chars()
        .map(|x| match x {
            '0' => '1',
            '1' => '1',
            _ => 'X',
        })
        .collect()
}

fn convert_mask_string_part2(mask: &str) -> u64 {
    let mask_string = mask
        .chars()
        .map(|x| match x {
            '0' => '0',
            '1' => '1',
            _ => '0',
        })
        .collect::<String>();
    //eprintln!("Mask {}", mask_string);
    u64::from_str_radix(&mask_string, 2).unwrap()
}

fn convert_mask_string(mask: &str) -> (u64, u64) {
    let zero_mask: String = mask
        .chars()
        .map(|x| match x {
            '0' => '0',
            _ => '1',
        })
        .collect();

    let one_mask: String = mask
        .chars()
        .map(|x| match x {
            '1' => '1',
            _ => '0',
        })
        .collect();

    (
        u64::from_str_radix(&zero_mask, 2).unwrap(),
        u64::from_str_radix(&one_mask, 2).unwrap(),
    )
}

#[test]
fn test_line_parse() {
    let lines = read_file("test").unwrap();
    let mut lines = lines.lines();
    let mut memory = Memory::new();

    memory.parse_line_part_1(lines.next().unwrap());
    assert_eq!(memory.cur_mask_one, 64);
    assert_eq!(
        memory.cur_mask_zero,
        u64::from_str_radix("111111111111111111111111111111111101", 2).unwrap()
    );

    memory.parse_line_part_1(lines.next().unwrap());
    assert_eq!(*memory.mem.get(&8).unwrap(), 73);

    memory.parse_line_part_1(lines.next().unwrap());
    assert_eq!(*memory.mem.get(&7).unwrap(), 101);

    memory.parse_line_part_1(lines.next().unwrap());
    assert_eq!(*memory.mem.get(&8).unwrap(), 64);
}

#[test]
fn test_solve1() {
    let lines = read_file("test").unwrap();
    let mut memory = Memory::new();

    assert_eq!(solve1(&mut memory, &lines), 165);
}

#[test]
fn test_floating_masks() {
    let mut memory = Memory::new();

    memory.parse_line_part_2("mask = 0X1X");
    assert_eq!(memory.mask_vec, vec![(2, 10), (3, 11), (6, 14), (7, 15)]);
}

#[test]
fn test_solve2() {
    let lines = read_file("test2").unwrap();
    let mut memory = Memory::new();

    assert_eq!(solve2(&mut memory, &lines), 208);
}
