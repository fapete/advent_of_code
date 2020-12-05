use day5::aoclib::*;

fn main() {
    let mut list_of_ids = get_all_ids(read_file("input").unwrap());
    println!("Maximal Seat ID: {}", solve1(&list_of_ids));

    // Part 2
    list_of_ids.sort_unstable();
    let mut idx = 0;
    while list_of_ids[idx] == list_of_ids[idx + 1] - 1 {
        idx += 1;
    }
    println! {"At index {}: Seat ID: {}, next: {}, thus our seat: {}", idx, list_of_ids[idx], list_of_ids[idx+1], list_of_ids[idx]+1};
}

#[derive(Copy, Debug, Clone)]
enum Dir {
    FL,
    BR,
}

fn binary_partition(dirs: Vec<Dir>, mut bounds: (u8, u8)) -> u8 {
    for dir in dirs {
        bounds = partition_step(dir, bounds);
    }
    assert_eq!(bounds.0, bounds.1);
    bounds.0
}

fn partition_step(d: Dir, (mut l, mut r): (u8, u8)) -> (u8, u8) {
    match d {
        Dir::FL => r = (r + l) / 2,
        Dir::BR => l = (r + l) / 2 + if (r + l) % 2 == 1 { 1 } else { 0 },
    }
    (l, r)
}

fn into_dirs(partition: &str) -> Vec<Dir> {
    partition
        .chars()
        .map(|c| {
            if c == 'F' || c == 'L' {
                Dir::FL
            } else {
                Dir::BR
            }
        })
        .collect()
}

fn get_all_ids(partitions: String) -> Vec<u64> {
    partitions
        .lines()
        .map(into_dirs)
        .map(|part| {
            let row = binary_partition(part.iter().take(7).cloned().collect(), (0, 127));
            let col = binary_partition(part.iter().skip(7).cloned().collect(), (0, 7));
            row as u64 * 8 + col as u64
        })
        .collect()
}

fn solve1(ids: &Vec<u64>) -> u64 {
    *ids.iter().max().unwrap()
}

#[test]
fn test_bin_part() {
    assert_eq!(binary_partition(into_dirs("BFFFBBF"), (0, 127)), 70);
    assert_eq!(binary_partition(into_dirs("RRR"), (0, 7)), 7);
    assert_eq!(binary_partition(into_dirs("FFFBBBF"), (0, 127)), 14);
    assert_eq!(binary_partition(into_dirs("BBFFBBF"), (0, 127)), 102);
    assert_eq!(binary_partition(into_dirs("RLL"), (0, 7)), 4);
}

#[test]
fn test_partition_step() {
    assert_eq!(partition_step(Dir::FL, (0, 127)), (0, 63));
    assert_eq!(partition_step(Dir::BR, (0, 63)), (32, 63));
    assert_eq!(partition_step(Dir::FL, (32, 63)), (32, 47));
    assert_eq!(partition_step(Dir::BR, (32, 47)), (40, 47));
    assert_eq!(partition_step(Dir::BR, (40, 47)), (44, 47));
    assert_eq!(partition_step(Dir::FL, (44, 47)), (44, 45));
    assert_eq!(partition_step(Dir::FL, (44, 45)), (44, 44));
}

#[test]
fn test_solve1() {
    assert_eq!(
        solve1(&get_all_ids(String::from(
            "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL\n"
        ))),
        820
    );
}
