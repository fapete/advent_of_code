use day10::aoclib::*;

fn main() {
    let deltas = get_deltas("input");
    println!("Part 1: {}", solve1(&deltas));
    println!("Part 2: {}", solve2(&deltas));
}

fn solve2(deltas: &Vec<u64>) -> u64 {
    let mut one_chains: Vec<u64> = vec![0];
    for i in deltas {
        if *i == 1 {
            if let Some(last) = one_chains.last_mut() {
                *last += 1;
            }
        }
        if *i == 3 {
            one_chains.push(0);
        }
    }
    //eprintln!("{:?}", one_chains);

    one_chains
        .iter()
        .filter(|x| **x > 0)
        .map(|x| {
            if *x <= 2 {
                *x
            } else if *x == 3 {
                4_u64
            } else {
                2_u64.pow(*x as u32 - 1) - 2_u64.pow(*x as u32 - 4)
            }
        })
        .product()
}

fn solve1(deltas: &Vec<u64>) -> u64 {
    deltas.iter().filter(|x| **x == 1).count() as u64
        * deltas.iter().filter(|x| **x == 3).count() as u64
}

fn get_deltas(infile: &str) -> Vec<u64> {
    let mut nums: Vec<_> = read_file(infile)
        .unwrap()
        .lines()
        .map(|x| x.parse::<u64>().unwrap())
        .collect();
    nums.sort_unstable();

    let mut deltas = Vec::new();
    deltas.push(nums[0]);
    for i in 0..nums.len() - 1 {
        let new_delta = nums[i + 1] - nums[i];
        deltas.push(new_delta);
    }
    deltas.push(3); // Device always adds another 3
    deltas
}

#[test]
fn test_solve1() {
    let deltas = get_deltas("test1");
    assert_eq!(solve1(&deltas), 7 * 5);
    let deltas = get_deltas("test2");
    assert_eq!(solve1(&deltas), 22 * 10);
}

#[test]
fn test_solve2() {
    let deltas = get_deltas("test1");
    assert_eq!(solve2(&deltas), 8);
    let deltas = get_deltas("test2");
    assert_eq!(solve2(&deltas), 19208);
}
