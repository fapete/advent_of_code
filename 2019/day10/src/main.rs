use fraction::Ratio;
use petgraph::visit::{IntoNodeReferences, NodeIndexable};
use petgraph::{Graph, Undirected};
use std::collections::HashMap;

type Fraction = Ratio<i32>;
type UGraph = Graph<Node, (), Undirected>;

#[derive(Debug, Clone, Copy)]
struct Node {
    x: i32,
    y: i32,
}

impl Node {
    fn new(x: i32, y: i32) -> Node {
        Node { x, y }
    }

    fn dist(&self, other: &Self) -> i32 {
        // We sign the distance: negative distance means other is below self, positive self below
        // other (if both are on the same y-coordinate, compare x accordingly)
        let sign = if self.y > other.y || (self.y == other.y && self.x > other.x) {
            1
        } else {
            -1
        };
        // L1-norm should suffice
        sign * ((self.x - other.x).abs() + (self.y - other.y).abs())
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Line {
    m: Fraction,
    b: Fraction,
}

impl Line {
    fn from(p1: &Node, p2: &Node) -> Line {
        if p1.x == p2.x {
            // Line is parallel to y-axis and cannot be given as a linear function.
            // Since there are no negative coordinates and every node can only have one such line,
            // we use m = b = -1 for this case, in a bit of an abuse of maths.
            Line {
                m: Fraction::new(-1, 1),
                b: Fraction::new(-1, 1),
            }
        } else {
            Line {
                m: Fraction::new((p2.y - p1.y).abs(), (p2.x - p1.x).abs()),
                b: Fraction::new((p2.x * p1.y - p1.x * p2.y).abs(), (p2.x - p1.x).abs()),
            }
        }
    }
}

#[derive(Debug)]
struct ClosestVisible {
    below: Option<(i32, usize)>,
    above: Option<(i32, usize)>,
}

impl ClosestVisible {
    fn new(d: i32, n: usize) -> ClosestVisible {
        if d < 0 {
            ClosestVisible {
                below: Some((d, n)),
                above: None,
            }
        } else {
            ClosestVisible {
                below: None,
                above: Some((d, n)),
            }
        }
    }

    fn update(&mut self, d: i32, n: usize) {
        if d < 0 {
            match self.below {
                None => self.below = Some((d, n)),
                Some((i, _)) => {
                    if i < d {
                        self.below = Some((d, n));
                    }
                }
            }
        } else {
            match self.above {
                None => self.above = Some((d, n)),
                Some((i, _)) => {
                    if i > d {
                        self.above = Some((d, n));
                    }
                }
            }
        }
    }
}

fn parse_grid(grid: &str) -> UGraph {
    let mut cur_x = 0;
    let mut cur_y = 0;
    let mut g = UGraph::new_undirected();

    // First read in nodes
    for line in grid.lines() {
        for c in line.chars() {
            if c == '#' {
                g.add_node(Node::new(cur_x, cur_y));
            }
            cur_x += 1;
        }
        cur_y += 1;
        cur_x = 0;
    }

    // Then add the edges. One edge to every visible node.
    let nodes: Vec<_> = g.node_references().collect();
    let mut add_edges = HashMap::new();
    for (id_u, u) in nodes {
        let mut lines = HashMap::new();
        for (id_v, v) in g.node_references().filter(|(id, _)| id_u != *id) {
            let dist = u.dist(v);
            let line = Line::from(u, v);
            lines
                .entry(line)
                .and_modify(|x: &mut ClosestVisible| x.update(dist, g.to_index(id_v)))
                .or_insert(ClosestVisible::new(dist, g.to_index(id_v)));
        }
        for (_, cv) in lines {
            if let Some((_, id)) = cv.below {
                add_edges
                    .entry(g.to_index(id_u))
                    .and_modify(|x: &mut Vec<_>| x.push(id))
                    .or_insert(vec![id]);
            }
            if let Some((_, id)) = cv.above {
                add_edges
                    .entry(g.to_index(id_u))
                    .and_modify(|x: &mut Vec<_>| x.push(id))
                    .or_insert(vec![id]);
            }
        }
    }
    for (u, vs) in add_edges {
        for v in vs {
            g.update_edge(g.from_index(u), g.from_index(v), ());
        }
    }

    g
}

fn main() {
    let grid = include_str!("../input");
    let graph = parse_grid(grid);
    // part 1:
    let node_of_highest_degree = graph
        .node_indices()
        .max_by(|x, y| {
            graph
                .neighbors(*x)
                .count()
                .cmp(&graph.neighbors(*y).count())
        })
        .unwrap();
    println!(
        "Most neighbors: {} ({:?})",
        graph.neighbors(node_of_highest_degree).count(),
        graph[node_of_highest_degree]
    );
    // Part 2:
    // get Nodes per quadrant
    let ur: Vec<_> = graph
        .neighbors(node_of_highest_degree)
        .filter(|other| {
            graph[*other].x >= graph[node_of_highest_degree].x
                && graph[*other].y <= graph[node_of_highest_degree].y
        })
        .map(|x| graph[x])
        .collect();
    let lr: Vec<_> = graph
        .neighbors(node_of_highest_degree)
        .filter(|other| {
            graph[*other].x >= graph[node_of_highest_degree].x
                && graph[*other].y > graph[node_of_highest_degree].y
        })
        .map(|x| graph[x])
        .collect();
    let ll: Vec<_> = graph
        .neighbors(node_of_highest_degree)
        .filter(|other| {
            graph[*other].x < graph[node_of_highest_degree].x
                && graph[*other].y >= graph[node_of_highest_degree].y
        })
        .map(|x| graph[x])
        .collect();
    let ul: Vec<_> = graph
        .neighbors(node_of_highest_degree)
        .filter(|other| {
            graph[*other].x < graph[node_of_highest_degree].x
                && graph[*other].y < graph[node_of_highest_degree].y
        })
        .map(|x| graph[x])
        .collect();
    eprintln!("Quadrant 1:");
    let node_count = vaporize_nodes(ur, graph[node_of_highest_degree], 0);
    eprintln!("Quadrant 2:");
    let node_count = vaporize_nodes(lr, graph[node_of_highest_degree], node_count);
    eprintln!("Quadrant 3:");
    let node_count = vaporize_nodes(ll, graph[node_of_highest_degree], node_count);
    eprintln!("Quadrant 4:");
    vaporize_nodes(ul, graph[node_of_highest_degree], node_count);
}

fn vaporize_nodes(nodes: Vec<Node>, center: Node, mut count: usize) -> usize {
    let mut nodes_with_rad: Vec<_> = nodes
        .iter()
        .map(|x| (x, ((x.y - center.y) as f64).atan2((x.x - center.x) as f64)))
        .collect();
    nodes_with_rad.sort_unstable_by(|x, y| x.1.partial_cmp(&y.1).unwrap());
    for n in nodes_with_rad {
        eprintln!("Vaporizing: {:?}", n);
        count += 1;
        if count == 200 {
            println!("Answer: {}", n.0.x * 100 + n.0.y);
        }
    }
    count
}
