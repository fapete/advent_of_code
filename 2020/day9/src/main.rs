use day9::aoclib::*;
use std::collections::HashMap;
use std::collections::VecDeque;

fn main() {
    let first_invalid = solve1("input", 25);
    println!("First invalid number: {}", first_invalid);
    println!("Encryption weakness: {}", solve2("input", first_invalid));
}

struct Slider {
    numbers: Vec<u64>,
    window_size: usize,
    sums_from: Vec<u64>,
}

impl Slider {
    fn from(num_list: String) -> Slider {
        let numbers: Vec<u64> = num_list
            .lines()
            .map(|line| line.parse::<u64>().unwrap())
            .collect();
        let window_size = 2;
        let mut sums_from = numbers.clone();
        for i in 0..numbers.len() - 1 {
            sums_from[i] += numbers[i + 1]
        }

        Slider {
            numbers,
            window_size,
            sums_from,
        }
    }

    fn find(&mut self, target: u64) -> u64 {
        loop {
            for i in 0..self.numbers.len() - self.window_size {
                self.sums_from[i] += self.numbers[i + self.window_size];
                if self.sums_from[i] == target {
                    eprintln!("{:?}", &self.numbers[i..i + self.window_size + 1]);
                    return find_min_max(&self.numbers[i..i + self.window_size + 1]);
                }
            }
            self.window_size += 1;
        }
    }
}

fn find_min_max(slice: &[u64]) -> u64 {
    let mut min = slice[0];
    let mut max = slice[0];
    for i in slice[1..].iter() {
        if i < &min {
            min = *i
        }
        if i > &max {
            max = *i
        }
    }
    min + max
}

#[derive(Debug)]
struct Code {
    current_nums: VecDeque<u64>,
    preamble_size: usize,
    valid_next: HashMap<u64, u64>,
}

impl Code {
    fn new(preamble_size: usize) -> Code {
        Code {
            current_nums: VecDeque::new(),
            preamble_size,
            valid_next: HashMap::new(),
        }
    }

    fn try_add_num(&mut self, num: u64) -> Result<(), u64> {
        // Only add num, if it is valid
        if self.current_nums.len() == self.preamble_size {
            match &self.valid_next.get(&num) {
                None => Err(num), // Number is not valid, return it in Error-State
                Some(_) => {
                    // Number is valid, add it, remove one if applicable, adjust next valid
                    if self.current_nums.len() == self.preamble_size {
                        // Remove first entry of the VecDeque
                        let out_num = self.current_nums.pop_front().unwrap();
                        for i in self.current_nums.iter() {
                            if *i != out_num {
                                self.valid_next
                                    .entry(out_num + i)
                                    .and_modify(|e| *e -= 1)
                                    .or_insert(0); // Insert should never happen here, but needed to resolve Entry
                                if *self.valid_next.get(&(out_num + i)).unwrap() == 0 {
                                    self.valid_next.remove(&(out_num + i));
                                }
                            }
                        }
                    }
                    // Add valid numbers for every entry already in Vector
                    for i in self.current_nums.iter() {
                        if *i != num {
                            self.valid_next
                                .entry(num + i)
                                .and_modify(|e| *e += 1)
                                .or_insert(1);
                        }
                    }
                    // Finally, add the new number to the vector
                    self.current_nums.push_back(num);
                    Ok(())
                }
            }
        } else {
            // Add valid numbers for every entry already in Vector
            for i in self.current_nums.iter() {
                if *i != num {
                    self.valid_next
                        .entry(num + i)
                        .and_modify(|e| *e += 1)
                        .or_insert(1);
                }
            }
            // Finally, add the new number to the vector
            self.current_nums.push_back(num);
            Ok(())
        }
    }
}

fn solve1(infile: &str, preamble_size: usize) -> u64 {
    let mut code = Code::new(preamble_size);
    for line in read_file(infile).unwrap().lines() {
        if let Err(n) = code.try_add_num(line.parse::<u64>().unwrap()) {
            return n;
        }
    }
    0
}

fn solve2(infile: &str, target_num: u64) -> u64 {
    Slider::from(read_file(infile).unwrap()).find(target_num)
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test", 5), 127);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test", 127), 62);
}
