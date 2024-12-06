#include <iostream>
#include <fstream>
#include <set>
#include <vector>
#include <string>
#include <utility>
#include <algorithm>

using Position = std::pair<int, int>;

enum Direction {UP, DOWN, LEFT, RIGHT};

struct GridActor {
    Position pos;
    Direction dir;

    void move() {
        switch (dir) {
            case UP:
                pos.second--;
                break;
            case DOWN:
                pos.second++;
                break;
            case LEFT:
                pos.first--;
                break;
            case RIGHT:
                pos.first++;
                break;
        }
    }

    void reverse_move() {
        turn_right();
        turn_right();
        move();
        turn_right();
        turn_right();
    }

    void turn_right() {
        switch (dir) {
            case UP:
                dir = RIGHT;
                break;
            case DOWN:
                dir = LEFT;
                break;
            case LEFT:
                dir = UP;
                break;
            case RIGHT:
                dir = DOWN;
                break;
        }
    }

    void turn_left() {
        turn_right();
        turn_right();
        turn_right();
    }
};

struct Guard: GridActor {};

std::ostream& operator<<(std::ostream& os, const Position& pos) {
    os << "(" << pos.first << ", " << pos.second << ")";
    return os;
}

std::ostream& operator<<(std::ostream& os, const Direction& dir) {
    switch (dir) {
        case UP:
            os << "UP";
            break;
        case DOWN:
            os << "DOWN";
            break;
        case LEFT:
            os << "LEFT";
            break;
        case RIGHT:
            os << "RIGHT";
            break;
    }

    return os;
}

std::ostream& operator<<(std::ostream& os, const GridActor actor) {
    os << "Actor at " << actor.pos << " facing " << actor.dir;
    return os;
}

template <typename TileType, typename ActorType>
struct Grid {
    using TileParse = TileType(*)(char);
    using ActorParse = ActorType(*)(char, Position);
    
private:
    std::vector<TileType> ts;
    std::vector<ActorType> as;
    TileType aTile;
    int r;
    int c;

    Position to_grid_position(const size_t index) const {
        return {index % c, index / c};
    }

    size_t from_grid_position(const Position& pos) const {
        int row = pos.second;
        int col = pos.first;

        return row * c + col;
    }

public:
    Grid(const std::vector<TileType>& tiles, const std::vector<ActorType> actors, int rows, int cols, TileType actorTile): ts{tiles}, as{actors}, r{rows}, c{cols}, aTile{actorTile} {
        static_assert(std::is_base_of<GridActor, ActorType>::value, "ActorType must be a subclass of GridActor");
    }

    static Grid from(std::istream& input, TileParse tileParser, ActorParse actorParser, TileType actor) {
        std::vector<TileType> tiles;
        int rows{0};
        int cols{0};
        char c;
        std::vector<ActorType> actors;

        while (input.get(c)) {
            if (c == '\n') {
                rows++;
                cols = 0;
            }
            else {
                TileType t = tileParser(c);
                if (t == actor) {
                    actors.push_back(actorParser(c, {cols, rows}));
                }
                tiles.push_back(t);
                cols++;
            }
        }

        cols = tiles.size() / rows;
        return Grid{tiles, actors, rows, cols, actor};
    }

    TileType& at(Position pos) {
        return ts[from_grid_position(pos)];
    }

    const TileType& at(Position pos) const {
        return ts[from_grid_position(pos)];
    }

    std::vector<ActorType>& actors() {
        return as;
    }

    int rows() const {
        return r;
    }

    int cols() const {
        return c;
    }

    ActorType& get_actor(size_t index) {
        return as[index];
    }

    bool is_in_bounds(Position pos) {
        int row = pos.second;
        int col = pos.first;

        return row >= 0 && row < r && col >= 0 && col < c;
    }

    // Returns false, if actor faces an obstacle
    bool can_move(ActorType actor, std::vector<TileType> obstacles) {
        //std::cout << actor;
        actor.move();
        if (std::find(obstacles.begin(), obstacles.end(), at(actor.pos)) != obstacles.end()) {
            //std::cout << " cannot move, because of grid tile " << at(actor.pos) << std::endl;
            actor.reverse_move();
            return false;
        }
        
        //std::cout << " can move." << std::endl;
        return true;
    }

    // Assumes actor can make the move, returns false if actor moved off grid
    bool move_actor(ActorType& actor) {
        //std::cout << "Moving actor " << actor << std::endl;
        actor.move();
        //std::cout << "New position " << actor;
    
        if (is_in_bounds(actor.pos)) {
            //std::cout << " is in bounds." << std::endl;
            return true;
        }

        //std::cout << " is not in bounds." << std::endl;
        return false;
    }

    void set_tile_at(Position pos, TileType tile) {
        ts[from_grid_position(pos)] = tile;
    }

    void set_actor_to(Position pos, Direction dir, ActorType actor, TileType emptyTile) {
        ts[from_grid_position(actor.pos)] = emptyTile;
        ts[from_grid_position(pos)] = aTile;
        actor.pos = pos;
        actor.dir = dir;
    }

    void set_tile_at(typename std::vector<TileType>::iterator it, TileType tile) {
        *it = tile;
    }

    typename std::vector<TileType>::iterator begin() {
        return ts.begin();
    }

    typename std::vector<TileType>::iterator end() {
        return ts.end();
    }
};

enum Tile {EMPTY, GUARD, OBSTACLE};

std::ostream& operator<<(std::ostream& os, const Tile& tile) {
    switch (tile) {
        case EMPTY:
            os << ".";
            break;
        case GUARD:
            os << "g";
            break;
        case OBSTACLE:
            os << "#";
            break;
    }

    return os;
}

std::ostream& operator<<(std::ostream& os, const Grid<Tile, Guard>& grid) {
    for (int row = 0; row < grid.rows(); row++) {
        for (int col = 0; col < grid.cols(); col++) {
            os << grid.at({col, row});
        }
        os << std::endl;
    }

    return os;
}

Tile parse_tile(char c) {
    switch (c) {
        case '>':
        case '<':
        case 'v':
        case '^':
            return GUARD;
        case '#':
            return OBSTACLE;
        default:
            return EMPTY;
    }
}

Guard parse_guard(char c, Position pos) {
    Direction dir;
    switch (c) {
        case '>':
            dir = Direction::RIGHT;
            break;
        case '<':
            dir = Direction::LEFT;
            break;
        case 'v':
            dir = Direction::DOWN;
            break;
        case '^':
            dir = Direction::UP;
            break;
    }

    Guard g{};
    g.pos = pos;
    g.dir = dir;

    return g;
}

Grid<Tile, Guard> parse_input(std::string filename) {
    std::ifstream file{filename};
    return Grid<Tile, Guard>::from(file, parse_tile, parse_guard, Tile::GUARD);
}

std::set<Position> part1(Grid<Tile, Guard>& grid) {
    std::vector<Tile> obstacles{OBSTACLE};
    std::set<Position> visited{grid.actors()[0].pos};

    Guard guard = grid.get_actor(0);
    Position initial_pos = guard.pos;
    Direction initial_dir = guard.dir;

    while (true) {
        if (!grid.can_move(guard, obstacles)) {
            guard.turn_right();
        }
        if (!grid.move_actor(guard)) {
            grid.set_actor_to(initial_pos, initial_dir, guard, EMPTY);
            return visited;
        }
        visited.insert(guard.pos);
    }
}

bool guard_loops(Grid<Tile, Guard>& grid) {
    std::vector<Tile> obstacles{OBSTACLE};
    Guard guard = grid.get_actor(0);

    Position initial_pos = guard.pos;
    Direction initial_dir = guard.dir;
    std::set<std::pair<Position, Direction>> visited_states{{initial_pos, initial_dir}};

    while (true) {
        if (!grid.can_move(guard, obstacles)) {
            guard.turn_right();
            continue;
        }

        if (!grid.move_actor(guard)) {
            grid.set_actor_to(initial_pos, initial_dir, guard, EMPTY);
            return false;
        }

        if (std::find(visited_states.begin(), visited_states.end(), std::make_pair(guard.pos, guard.dir)) != visited_states.end()) {
            grid.set_actor_to(initial_pos, initial_dir, guard, EMPTY);
            return true;
        }

        visited_states.insert({guard.pos, guard.dir});
    }
}

int part2(Grid<Tile, Guard>& grid, std::set<Position> potential_obstacles) {
    int count{0};

    for (auto pos: potential_obstacles) {
        grid.set_tile_at(pos, OBSTACLE);
        if (guard_loops(grid)) {
            count++;
        }
        grid.set_tile_at(pos, EMPTY);
    }

    return count;
}

int main() {
    std::string filename;
    std::cin >> filename;

    Grid<Tile, Guard> grid = parse_input(filename);
    std::set<Position> p1 = part1(grid);
    std::cout << p1.size() << std::endl;
    int p2 = part2(grid, p1);
    std::cout << p2 << std::endl;
}