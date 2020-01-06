use petgraph::visit::NodeIndexable;
use petgraph::{Directed, Graph};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

type GridPos = (u32, u32);
type Grid = HashMap<GridPos, Tile>;

#[derive(PartialEq, Eq, Hash)]
enum Tile {
    Wall,
    Floor,
    Key(char),
    Door(char),
    Character,
}

impl Tile {
    fn is_floor(&self) -> bool {
        match self {
            Tile::Floor => true,
            Tile::Character => true,
            Tile::Key(_) => true,
            _ => false,
        }
    }
}

struct Map {
    grid: Grid,
    char_pos: GridPos,
    key_count: u32,
}

impl Map {
    fn from(input: String) -> Map {
        let mut grid = Grid::new();

        let mut x = 0;
        let mut y = 0;
        let mut char_pos = (0, 0);
        let mut key_count = 0;
        for line in input.lines() {
            for c in line.chars() {
                let tile = match c {
                    '.' => Tile::Floor,
                    '#' => Tile::Wall,
                    '@' => {
                        char_pos = (x, y);
                        Tile::Character
                    }
                    _ => {
                        if c.is_lowercase() {
                            key_count += 1;
                            Tile::Key(c)
                        } else {
                            Tile::Door(c.to_ascii_lowercase())
                        }
                    }
                };
                grid.insert((x, y), tile);
                x += 1;
            }
            x = 0;
            y += 1;
        }

        Map {
            grid,
            char_pos,
            key_count,
        }
    }

    fn adjacent(
        &self,
        (x, y): GridPos,
        treat_as_open: Option<&char>,
        collected_keys: &HashSet<char>,
    ) -> Vec<GridPos> {
        vec![(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            .iter()
            .cloned()
            .filter(|x| match self.grid.get(x) {
                Some(t) => {
                    if let Tile::Door(c) = t {
                        collected_keys.contains(c) || treat_as_open == Some(c)
                    } else {
                        t.is_floor()
                    }
                }
                None => false,
            })
            .collect()
    }

    fn key_for(&self, pos: &GridPos) -> Option<char> {
        if let Some(Tile::Key(c)) = self.grid.get(pos) {
            Some(*c)
        } else {
            None
        }
    }
}

fn uncollected_keys_reachable_from(
    map: &Map,
    from: GridPos,
    with_collected: &HashSet<char>,
) -> Vec<(GridPos, u32)> {
    let mut res = Vec::new();

    // run a bfs starting at from, add all keys we find, and ignore the door matching key at
    // from or any doors already opened.
    let open_door = if let Some(Tile::Key(c)) = map.grid.get(&from) {
        Some(c)
    } else {
        None
    };

    let mut to_visit = VecDeque::new();
    to_visit.push_back((from, 0));
    let mut seen = HashSet::new();
    while let Some((p, i)) = to_visit.pop_front() {
        for n in map
            .adjacent(p, open_door, with_collected)
            .iter()
            .filter(|x| !seen.contains(*x))
        {
            to_visit.push_back((*n, i + 1));
        }
        seen.insert(p);
        let tile = &map.grid[&p];
        match tile {
            Tile::Key(c) => {
                if !with_collected.contains(c) {
                    res.push((p, i))
                }
            }
            _ => (),
        }
    }

    res
}

#[derive(Debug)]
struct Node {
    pos: GridPos,
    collected: HashSet<char>,
}

impl Node {
    fn from(pos: GridPos, collected: HashSet<char>) -> Node {
        Node { pos, collected }
    }
}

fn build_graph(map: &Map) -> Graph<Node, u32, Directed> {
    let mut graph = Graph::new();
    //    let mut sink_nodes = Vec::new();
    let mut nodes_to_visit = VecDeque::new();

    let init_pos = map.char_pos;
    //        .grid
    //        .iter()
    //        .find(|(_, t)| **t == Tile::Character)
    //        .unwrap()
    //        .0;
    let init_node = graph.add_node(Node::from(init_pos, HashSet::new()));
    let sink_node = graph.add_node(Node::from((0, 0), HashSet::new()));
    nodes_to_visit.push_back(init_node);

    while let Some(cur_node_idx) = nodes_to_visit.pop_front() {
        //        eprintln!("Processing Node: {:?}", graph[cur_node_idx]);
        // TODO: Check petgraph docs to see if you can get by without these clones.
        let pos = graph[cur_node_idx].pos.clone();
        let collected = graph[cur_node_idx].collected.clone();
        for (key_pos, dist) in uncollected_keys_reachable_from(map, pos, &collected) {
            let key = map.key_for(&key_pos).unwrap();
            let mut new_collected = collected.clone();
            new_collected.insert(key);
            let key_node = graph.add_node(Node::from(key_pos, new_collected));
            graph.add_edge(cur_node_idx, key_node, dist);
            nodes_to_visit.push_back(key_node);
        }

        if graph.neighbors(cur_node_idx).collect::<Vec<_>>().len() == 0 {
            graph.add_edge(cur_node_idx, sink_node, 0);
        }
    }

    graph
}

fn part1(input: &str) -> u32 {
    let map = Map::from(fs::read_to_string(input).unwrap());
    let graph = build_graph(&map);
    let source = graph.from_index(0);
    let sink = graph.from_index(1);
    let costs = petgraph::algo::dijkstra(&graph, source, Some(sink), |e| *e.weight());

    //    eprintln!("{:?}", graph);
    *costs.get(&sink).unwrap()
}

fn main() {
    println!("{}", part1("ex3"));
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_reachable_from() {
        let map = Map::from(fs::read_to_string("tiny_ex").unwrap());
        assert_eq!(
            uncollected_keys_reachable_from(&map, (5, 1), &HashSet::new()),
            vec![((7, 1), 2)]
        );
        let mut set = HashSet::new();
        set.insert('a');
        assert_eq!(
            uncollected_keys_reachable_from(&map, (7, 1), &set),
            vec![((1, 1), 6)]
        );
    }

    #[test]
    fn test_examples() {
        assert_eq!(part1("tiny_ex"), 8);
        assert_eq!(part1("ex1"), 86);
        assert_eq!(part1("ex2"), 132);
        assert_eq!(part1("ex3"), 136);
        assert_eq!(part1("ex4"), 81);
    }
}
