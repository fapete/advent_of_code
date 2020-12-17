use day17::aoclib::*;

fn main() {
    println!("Cubes alive after 6 cycles: {}", solve1("input"));
    println!("Cubes alive after 6 cycles in 4D: {}", solve2("input"));
}
struct CubeLife4 {
    cubes: Vec<Vec<Vec<Vec<bool>>>>,
    dimensions: (usize, usize, usize, usize),
}

type Pos4 = (i64, i64, i64, i64);

impl From<&str> for CubeLife4 {
    fn from(initial_slice: &str) -> CubeLife4 {
        let mut cubes = Vec::new();
        for (row, line) in initial_slice.lines().enumerate() {
            cubes.push(Vec::new());
            for (col, c) in line.chars().enumerate() {
                cubes[row].push(Vec::new());
                cubes[row][col].push(Vec::new());
                if c == '#' {
                    cubes[row][col][0].push(true);
                } else {
                    cubes[row][col][0].push(false);
                }
            }
        }

        let dimensions = (
            cubes.len(),
            cubes[0].len(),
            cubes[0][0].len(),
            cubes[0][0][0].len(),
        );
        CubeLife4 { cubes, dimensions }
    }
}

impl CubeLife4 {
    fn living_neighbors(&self, (x, y, z, w): Pos4) -> Vec<Pos4> {
        let mut result = Vec::new();

        for dx in vec![1, 0, -1] {
            for dy in vec![1, 0, -1] {
                for dz in vec![1, 0, -1] {
                    for dw in vec![1, 0, -1] {
                        let (cur_x, cur_y, cur_z, cur_w) = (x + dx, y + dy, z + dz, w + dw);
                        if 0 <= cur_x
                            && cur_x < self.cubes.len() as i64
                            && 0 <= cur_y
                            && cur_y < self.cubes[cur_x as usize].len() as i64
                            && 0 <= cur_z
                            && cur_z < self.cubes[cur_x as usize][cur_y as usize].len() as i64
                            && 0 <= cur_w
                            && cur_w
                                < self.cubes[cur_x as usize][cur_y as usize][cur_z as usize].len()
                                    as i64
                        {
                            if self.cubes[cur_x as usize][cur_y as usize][cur_z as usize]
                                [cur_w as usize]
                                && (cur_x, cur_y, cur_z, cur_w) != (x, y, z, w)
                            {
                                result.push((cur_x, cur_y, cur_z, cur_w));
                            }
                        }
                    }
                }
            }
        }

        result
    }

    fn is_alive(&self, (x, y, z, w): Pos4) -> bool {
        if 0 <= x
            && x < self.dimensions.0 as i64
            && 0 <= y
            && y < self.dimensions.1 as i64
            && 0 <= z
            && z < self.dimensions.2 as i64
            && 0 <= w
            && w < self.dimensions.3 as i64
        {
            self.cubes[x as usize][y as usize][z as usize][w as usize]
        } else {
            false
        }
    }

    fn cycle(&mut self) {
        let mut new_cubes: Vec<Vec<Vec<Vec<bool>>>> = Vec::new();
        let (dimx, dimy, dimz, dimw) = self.dimensions;
        for i in 0..dimx + 2 {
            new_cubes.push(Vec::new());
            for j in 0..dimy + 2 {
                new_cubes[i].push(Vec::new());
                for k in 0..dimz + 2 {
                    new_cubes[i][j].push(Vec::new());
                    new_cubes[i][j][k].resize(dimw + 2, false);
                }
            }
        }

        for x in 0..dimx + 2 {
            for y in 0..dimy + 2 {
                for z in 0..dimz + 2 {
                    for w in 0..dimw + 2 {
                        let neighbours = self.living_neighbors(convert_to_old4((
                            x as i64, y as i64, z as i64, w as i64,
                        )));
                        if self.is_alive(convert_to_old4((x as i64, y as i64, z as i64, w as i64)))
                            && (neighbours.len() == 2 || neighbours.len() == 3)
                        {
                            new_cubes[x][y][z][w] = true;
                        } else {
                            new_cubes[x][y][z][w] = false;
                        }
                        if !self.is_alive(convert_to_old4((x as i64, y as i64, z as i64, w as i64)))
                            && (neighbours.len() == 3)
                        {
                            new_cubes[x][y][z][w] = true;
                        }
                    }
                }
            }
        }

        self.cubes = new_cubes;
        self.dimensions = (dimx + 2, dimy + 2, dimz + 2, dimw + 2);
    }

    fn count_alive(&self) -> u64 {
        let mut result = 0;
        for row in self.cubes.iter() {
            for col in row.iter() {
                for c in col.iter() {
                    for val in c.iter() {
                        result += if *val { 1 } else { 0 };
                    }
                }
            }
        }
        result
    }

    /* fn print(&self) {
        for z in 0..self.dimensions.2 {
            eprintln!("z = {}", z);
            for row in 0..self.dimensions.0 {
                for col in 0..self.dimensions.1 {
                    eprint!("{}", if self.cubes[row][col][z] { '#' } else { '.' });
                }
                eprint!("\n")
            }
            eprint!("\n\n");
        }
    }*/
}

fn convert_to_old4((x, y, z, w): Pos4) -> Pos4 {
    (x - 1, y - 1, z - 1, w - 1)
}

struct CubeLife {
    cubes: Vec<Vec<Vec<bool>>>,
    dimensions: (usize, usize, usize),
}

type Pos = (i64, i64, i64);

impl From<&str> for CubeLife {
    fn from(initial_slice: &str) -> CubeLife {
        let mut cubes = Vec::new();
        for (row, line) in initial_slice.lines().enumerate() {
            cubes.push(Vec::new());
            for (col, c) in line.chars().enumerate() {
                cubes[row].push(Vec::new());
                if c == '#' {
                    cubes[row][col].push(true);
                } else {
                    cubes[row][col].push(false);
                }
            }
        }

        let dimensions = (cubes.len(), cubes[0].len(), cubes[0][0].len());
        CubeLife { cubes, dimensions }
    }
}

impl CubeLife {
    fn living_neighbors(&self, (x, y, z): Pos) -> Vec<Pos> {
        let mut result = Vec::new();

        for dx in vec![1, 0, -1] {
            for dy in vec![1, 0, -1] {
                for dz in vec![1, 0, -1] {
                    let (cur_x, cur_y, cur_z) = (x + dx, y + dy, z + dz);
                    if 0 <= cur_x
                        && cur_x < self.cubes.len() as i64
                        && 0 <= cur_y
                        && cur_y < self.cubes[cur_x as usize].len() as i64
                        && 0 <= cur_z
                        && cur_z < self.cubes[cur_x as usize][cur_y as usize].len() as i64
                    {
                        if self.cubes[cur_x as usize][cur_y as usize][cur_z as usize]
                            && (cur_x, cur_y, cur_z) != (x, y, z)
                        {
                            result.push((cur_x, cur_y, cur_z));
                        }
                    }
                }
            }
        }

        result
    }

    fn is_alive(&self, (x, y, z): Pos) -> bool {
        if 0 <= x
            && x < self.dimensions.0 as i64
            && 0 <= y
            && y < self.dimensions.1 as i64
            && 0 <= z
            && z < self.dimensions.2 as i64
        {
            self.cubes[x as usize][y as usize][z as usize]
        } else {
            false
        }
    }

    fn cycle(&mut self) {
        let mut new_cubes: Vec<Vec<Vec<bool>>> = Vec::new();
        let (dimx, dimy, dimz) = self.dimensions;
        for i in 0..dimx + 2 {
            new_cubes.push(Vec::new());
            for j in 0..dimy + 2 {
                new_cubes[i].push(Vec::new());
                new_cubes[i][j].resize(dimz + 2, false);
            }
        }

        for x in 0..dimx + 2 {
            for y in 0..dimy + 2 {
                for z in 0..dimz + 2 {
                    let neighbours =
                        self.living_neighbors(convert_to_old((x as i64, y as i64, z as i64)));
                    if self.is_alive(convert_to_old((x as i64, y as i64, z as i64)))
                        && (neighbours.len() == 2 || neighbours.len() == 3)
                    {
                        new_cubes[x][y][z] = true;
                    } else {
                        new_cubes[x][y][z] = false;
                    }
                    if !self.is_alive(convert_to_old((x as i64, y as i64, z as i64)))
                        && (neighbours.len() == 3)
                    {
                        new_cubes[x][y][z] = true;
                    }
                }
            }
        }

        self.cubes = new_cubes;
        self.dimensions = (dimx + 2, dimy + 2, dimz + 2);
    }

    fn count_alive(&self) -> u64 {
        let mut result = 0;
        for row in self.cubes.iter() {
            for col in row.iter() {
                for c in col.iter() {
                    result += if *c { 1 } else { 0 };
                }
            }
        }
        result
    }

    fn print(&self) {
        for z in 0..self.dimensions.2 {
            eprintln!("z = {}", z);
            for row in 0..self.dimensions.0 {
                for col in 0..self.dimensions.1 {
                    eprint!("{}", if self.cubes[row][col][z] { '#' } else { '.' });
                }
                eprint!("\n")
            }
            eprint!("\n\n");
        }
    }
}

fn convert_to_old((x, y, z): Pos) -> Pos {
    (x - 1, y - 1, z - 1)
}

fn solve1(fname: &str) -> u64 {
    let mut cubelife = CubeLife::from(read_file(fname).unwrap().as_str());
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.count_alive()
}

fn solve2(fname: &str) -> u64 {
    let mut cubelife = CubeLife4::from(read_file(fname).unwrap().as_str());
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.cycle();
    //cubelife.print();
    cubelife.count_alive()
}

#[test]
fn test_solve1() {
    assert_eq!(solve1("test"), 112);
}

#[test]
fn test_solve2() {
    assert_eq!(solve2("test"), 848);
}
