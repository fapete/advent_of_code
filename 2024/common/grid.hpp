#include <iostream>
#include <set>
#include <vector>
#include <deque>
#include <string>
#include <utility>
#include <algorithm>
#include <iterator>

namespace grid {

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

        friend std::ostream& operator<<(std::ostream& os, const GridActor actor);
    };

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

    template<typename TileType>
    struct Grid {
        using TileParse = TileType(*)(char);
        using PositionFilter = std::function<bool(const Position&, const Position&)>;
    protected:
        std::vector<TileType> ts;
        int r;
        int c;

        Position to_grid_position(const size_t index) const {
            return {index % c, index / c};
        }

        Position to_grid_position(typename std::vector<TileType>::iterator it) const {
            return to_grid_position(it - ts.begin());
        }

        size_t from_grid_position(const Position& pos) const {
            int row = pos.second;
            int col = pos.first;

            return row * c + col;
        }

    public:
        Grid(const std::vector<TileType>& tiles, int rows, int cols): ts{tiles}, r{rows}, c{cols} {
        }

        static Grid from(std::istream& input, TileParse tileParser) {
            std::vector<TileType> tiles;
            int rows{0};
            int cols{0};
            char c;

            while (input.get(c)) {
                if (c == '\n') {
                    rows++;
                    cols = 0;
                }
                else {
                    TileType t = tileParser(c);
                    tiles.push_back(t);
                    cols++;
                }
            }

            cols = tiles.size() / rows;
            return Grid{tiles, rows, cols};
        }

        TileType& at(Position pos) {
            return ts[from_grid_position(pos)];
        }

        const TileType& at(Position pos) const {
            return ts[from_grid_position(pos)];
        }

        int rows() const {
            return r;
        }

        int cols() const {
            return c;
        }

        bool is_in_bounds(Position pos) const {
            int row = pos.second;
            int col = pos.first;

            return row >= 0 && row < r && col >= 0 && col < c;
        }

        void set_tile_at(Position pos, TileType tile) {
            ts[from_grid_position(pos)] = tile;
        }

        void set_tile_at(typename std::vector<TileType>::iterator it, TileType tile) {
            *it = tile;
        }

        std::vector<Position> neighbors(const Position& p) const {
            std::vector<Position> neighbors;
            Direction dirs[] = {UP, DOWN, LEFT, RIGHT};

            for (auto dir: dirs) {
                Position neighbor = p;
                switch (dir) {
                    case UP:
                        neighbor.second--;
                        break;
                    case DOWN:
                        neighbor.second++;
                        break;
                    case LEFT:
                        neighbor.first--;
                        break;
                    case RIGHT:
                        neighbor.first++;
                        break;
                }

                if (is_in_bounds(neighbor)) {
                    neighbors.push_back(neighbor);
                }
            }

            return neighbors;
        }

        std::set<Position> bfs(typename std::vector<TileType>::iterator start, const TileType& target, PositionFilter is_valid_position = [](const Position&, const Position&) {return true;}) const {
            Position startPos = to_grid_position(start);

            std::set<Position> seen;
            std::set<Position> result;
            std::deque<Position> queue{startPos};

            while (!queue.empty()) {
                Position current = queue.front();
                queue.pop_front();
                seen.insert(current);
                if (at(current) == target) result.insert(current);
                for (auto& neighbor: neighbors(current)) {
                    if (seen.find(neighbor) == seen.end() && is_valid_position(current, neighbor)) {
                        queue.push_back(neighbor);
                    }
                }
            }

            return result;
        }

        std::vector<std::vector<Position>> dfs(typename std::vector<TileType>::iterator start, const TileType& target, PositionFilter is_valid_position = [](const Position&, const Position&) {return true;}, bool find_all_paths = false) const {
            Position startPos = to_grid_position(start);

            std::set<Position> seen;
            return dfs(startPos, target, {}, seen, is_valid_position, find_all_paths);
        }

    protected:
        std::vector<std::vector<Position>> dfs(Position from, const TileType& target, std::vector<Position> currentPath, std::set<Position>& seen, PositionFilter is_valid_Position, bool find_all_paths) const {
            seen.insert(from);
            std::vector<std::vector<Position>> result;
            currentPath.push_back(from);

            if (at(from) == target) {
                result.push_back(currentPath);
            }

            for (auto& neighbor: neighbors(from)) {
                if (seen.find(neighbor) == seen.end() && is_valid_Position(from, neighbor)) {
                    if (find_all_paths) {
                        std::set<Position> new_seen{seen}; // Copy seen set so Paths can be partially shared (just don't go back)
                        auto paths = dfs(neighbor, target, currentPath, new_seen, is_valid_Position, find_all_paths);
                        result.insert(result.end(), paths.begin(), paths.end());
                    } else {
                        auto paths = dfs(neighbor, target, currentPath, seen, is_valid_Position, find_all_paths);
                        result.insert(result.end(), paths.begin(), paths.end());
                    }
                }
            }

            return result;
        }

    public:    

        typename std::vector<TileType>::iterator begin() {
            return ts.begin();
        }

        typename std::vector<TileType>::iterator end() {
            return ts.end();
        }

    };

    template <typename TileType, typename ActorType>
    struct ActorGrid: Grid<TileType> {
        using TileParse = typename Grid<TileType>::TileParse;
        using ActorParse = ActorType(*)(char, Position);
        
    protected:
        std::vector<ActorType> as;
        TileType aTile;

    public:
        ActorGrid(const std::vector<TileType>& tiles, const std::vector<ActorType> actors, int rows, int cols, TileType actorTile): Grid<TileType>{tiles, rows, cols}, as{actors}, aTile{actorTile} {
            static_assert(std::is_base_of<GridActor, ActorType>::value, "ActorType must be a subclass of GridActor");
        }

        static ActorGrid from(std::istream& input, TileParse tileParser, ActorParse actorParser, TileType actor) {
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
            return this->ts[this->from_grid_position(pos)];
        }

        const TileType& at(Position pos) const {
            return this->ts[this->from_grid_position(pos)];
        }

        std::vector<ActorType>& actors() {
            return as;
        }

        ActorType& get_actor(size_t index) {
            return as[index];
        }

        // Returns false, if actor faces an obstacle
        bool can_move(ActorType actor, std::vector<TileType> obstacles) {
            actor.move();
            if (std::find(obstacles.begin(), obstacles.end(), at(actor.pos)) != obstacles.end()) {
                actor.reverse_move();
                return false;
            }
            
            return true;
        }

        // Assumes actor can make the move, returns false if actor moved off grid
        bool move_actor(ActorType& actor) {
            actor.move();
        
            if (this->is_in_bounds(actor.pos)) {
                return true;
            }

            return false;
        }

        void set_actor_to(Position pos, Direction dir, ActorType actor, TileType emptyTile) {
            this->ts[this->from_grid_position(actor.pos)] = emptyTile;
            this->ts[this->from_grid_position(pos)] = aTile;
            actor.pos = pos;
            actor.dir = dir;
        }
    };
}
