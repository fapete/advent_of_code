use day3::aoclib::*;

fn main() {
    println!("Task 1: {}", solve_task1(read_file("input").unwrap()));
    println!("Task 2: {}", solve_task2(read_file("input").unwrap()));
}

struct Grid {
    // true: tree, false: empty
    grid: Vec<Vec<bool>>,
}

impl Grid {
    fn from(grid_desc: String) -> Grid {
        Grid {
            grid: grid_desc
                .lines()
                .map(|line| line.chars().map(|c| c == '#').collect())
                .collect(),
        }
    }

    fn num_trees_from(&self, slope_r: usize, slope_d: usize) -> u64 {
        let mut treecount = 0;
        let mut col = 0;

        for i in (slope_d..self.grid.len()).step_by(slope_d) {
            col = (col + slope_r) % self.grid[0].len();
            treecount += if self.grid[i][col] { 1 } else { 0 };
        }
        treecount
    }
}

fn solve_task1(grid: String) -> u64 {
    let g = Grid::from(grid);
    g.num_trees_from(3, 1)
}

fn solve_task2(grid: String) -> u64 {
    let g = Grid::from(grid);
    let mut slopes = Vec::new();
    slopes.push(g.num_trees_from(1, 1));
    slopes.push(g.num_trees_from(3, 1));
    slopes.push(g.num_trees_from(5, 1));
    slopes.push(g.num_trees_from(7, 1));
    slopes.push(g.num_trees_from(1, 2));
    slopes.iter().product()
}

#[test]
fn test_task1() {
    assert_eq!(solve_task1(read_file("test").unwrap()), 7);
    assert_eq!(solve_task1(read_file("test2").unwrap()), 3);
    assert_eq!(solve_task1(read_file("input").unwrap()), 292);
}

#[test]
fn test_task2() {
    assert_eq!(solve_task2(read_file("test").unwrap()), 336);
    assert_eq!(solve_task2(read_file("test2").unwrap()), 4 * 3 * 2 * 4 * 1);
}
