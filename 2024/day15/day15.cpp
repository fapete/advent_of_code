#include "../common/grid.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <utility>

using namespace grid;

enum ActorType { ROBOT, BOX, BOX_OPEN, BOX_CLOSE };

struct WarehouseActor: GridActor {
    ActorType type;
};

std::ostream& operator<<(std::ostream& os, const WarehouseActor& actor) {
    switch (actor.type) {
        case ROBOT:
            os << "@";
            break;
        case BOX:
            os << "O";
            break;
        case BOX_OPEN:
            os << "[";
            break;
        case BOX_CLOSE:
            os << "]";
            break;
    }
    return os;
}

WarehouseActor parse_actor(char c, Position pos) {
    switch (c) {
        case '@':
            return {pos, DOWN, ROBOT}; // Direction is irrelevant in this one
        case 'O':
            return {pos, DOWN, BOX};
        default:
            throw std::invalid_argument("Invalid actor type");
    }
}

enum WarehouseTiles { WALL, EMPTY, ACTOR };

void print_actor_grid(const ActorGrid<WarehouseTiles, WarehouseActor>& grid) {
    for (int y = 0; y < grid.rows(); y++) {
        for (int x = 0; x < grid.cols(); x++) {
            Position pos{x, y};
            auto actor = grid.get_actor(pos);
            if (actor != grid.actors().end()) {
                std::cout << *actor;
            } else {
                std::cout << (grid.at({x, y}) == WALL ? '#' : '.');
            }
        }
        std::cout << std::endl;
    }
}

WarehouseTiles parse_tile(char c) {
    switch (c) {
        case '#':
            return WALL;
        case '.':
            return EMPTY;
        case '@':
        case 'O':
            return ACTOR;
        default:
            throw std::invalid_argument("Invalid tile type");
    }
}

Direction parse_direction(char c) {
    switch (c) {
        case '^':
            return UP;
        case 'v':
            return DOWN;
        case '<':
            return LEFT;
        case '>':
            return RIGHT;
        default:
            throw std::invalid_argument("Invalid direction");
    }
}

ActorGrid<WarehouseTiles, WarehouseActor> parse_grid(std::istream& input) {
    std::string line;
    std::string grid;
    while (std::getline(input, line)) {
        if (line.empty() || line == "\n") {
            break;
        }
    
        grid += line;
        grid.push_back('\n');
    }

    std::istringstream iss{grid};
    return ActorGrid<WarehouseTiles, WarehouseActor>::from(iss, parse_tile, parse_actor, ACTOR);
}

std::vector<Direction> parse_directions(std::istream& input) {
    char c;
    std::vector<Direction> directions;
    while (input.get(c)) {
        try {
            Direction d = parse_direction(c);
            directions.push_back(d);
        } catch (std::invalid_argument& e) {
            // ignore on purpose
            continue;
        }
    }

    return directions;
}

std::pair<ActorGrid<WarehouseTiles, WarehouseActor>, std::vector<Direction>> parse_input(std::string fileName) {
    std::ifstream file{fileName};
    ActorGrid<WarehouseTiles, WarehouseActor> grid = parse_grid(file);
    std::vector<Direction> directions = parse_directions(file);

    return {grid, directions};
}

void simulate_move(ActorGrid<WarehouseTiles, WarehouseActor>& grid, WarehouseActor* robot, Direction dir) {
    std::vector<WarehouseActor*> actors_to_move{robot};
    Position next_pos = robot->pos;
    while (grid.at(next_pos) != WALL && grid.at(next_pos) != EMPTY) {
        next_pos = pos_in_dir(next_pos, dir);
        auto actor_it = grid.get_actor(next_pos);
        if (actor_it != grid.actors().end()) {
            actors_to_move.push_back(&*actor_it);
        }
    }

    if (grid.at(next_pos) == WALL) {
        return; // No move possible
    }

    for (auto it = actors_to_move.rbegin(); it != actors_to_move.rend(); it++) {
        grid.set_tile_at((*it)->pos, EMPTY);
        (*it)->move(dir);
        grid.set_tile_at((*it)->pos, ACTOR);
    }
}

void enlarged_simulate_move(ActorGrid<WarehouseTiles, WarehouseActor>& grid, WarehouseActor* robot, Direction dir) {
    std::set<WarehouseActor*> actors_to_move{robot};
    std::vector<WarehouseActor*> actors_to_move_in_order{robot};
    std::vector<Position> next_pos{robot->pos};
    while (std::all_of(next_pos.begin(), next_pos.end(), [&grid](Position p) {return grid.at(p) != WALL;}) &&
        std::any_of(next_pos.begin(), next_pos.end(), [&grid](Position p) {return grid.at(p) != EMPTY;})) {
        
        std::vector<Position> new_next_pos;
        for (auto p: next_pos) {
            Position new_pos = pos_in_dir(p, dir);
            auto actor_it = grid.get_actor(new_pos);
            Position new_pos2 = pos_in_dir(new_pos, dir);
            Position box_counter_pos;
            auto box_counter_it = grid.get_actor(new_pos2);
            if (actor_it != grid.actors().end()) {
                switch (actor_it->type) {
                    case BOX:
                        throw std::runtime_error("Non-Enlarged Box in enlarged grid");
                    case ROBOT:
                        throw std::runtime_error("Robot pushing itself?");
                    case BOX_OPEN:
                        box_counter_pos = pos_in_dir(actor_it->pos, RIGHT);
                        box_counter_it = grid.get_actor(box_counter_pos);
                        if (box_counter_it == grid.actors().end()) {
                            throw new std::runtime_error("Open box without close box right of it");
                        }
                        if (actors_to_move.find(&*box_counter_it) == actors_to_move.end()) {
                            actors_to_move.insert(&*actor_it);
                            actors_to_move.insert(&*box_counter_it);
                            actors_to_move_in_order.push_back(&*actor_it);
                            actors_to_move_in_order.push_back(&*box_counter_it);
                            if (dir == LEFT || dir == RIGHT) {
                                new_next_pos.push_back(new_pos2);
                            } else {
                                new_next_pos.push_back(new_pos);
                                new_next_pos.push_back(box_counter_pos);
                            }
                        }
                        break;
                    case BOX_CLOSE:
                        box_counter_pos = pos_in_dir(actor_it->pos, LEFT);
                        box_counter_it = grid.get_actor(box_counter_pos);
                        if (box_counter_it == grid.actors().end()) {
                            throw new std::runtime_error("Close box without open box left of it");
                        }
                        if (actors_to_move.find(&*box_counter_it) == actors_to_move.end()) {
                            actors_to_move.insert(&*actor_it);
                            actors_to_move.insert(&*box_counter_it);
                            actors_to_move_in_order.push_back(&*actor_it);
                            actors_to_move_in_order.push_back(&*box_counter_it);
                            if (dir == LEFT || dir == RIGHT) {
                                new_next_pos.push_back(new_pos2);
                            } else {
                                new_next_pos.push_back(new_pos);
                                new_next_pos.push_back(box_counter_pos);
                            }
                        }
                        break;
                }
            } else {
                if (grid.at(new_pos) == WALL) {
                    new_next_pos.push_back(new_pos);
                }
            }
        }
        next_pos = new_next_pos;
    }

    if (std::any_of(next_pos.begin(), next_pos.end(), [&grid] (Position p) {return grid.at(p) == WALL;})) {
        return; // No move possible
    }

    for (auto it = actors_to_move_in_order.rbegin(); it != actors_to_move_in_order.rend(); it++) {
        grid.set_tile_at((*it)->pos, EMPTY);
        (*it)->move(dir);
        grid.set_tile_at((*it)->pos, ACTOR);
    }
}

long part1(ActorGrid<WarehouseTiles, WarehouseActor>& grid, const std::vector<Direction>& directions) {
    WarehouseActor* robot = &*grid.find_actor_by([](WarehouseActor& actor) {return actor.type == ROBOT;});

    //std::cout << "Initial state" << std::endl;
    //print_actor_grid(grid);
    for (auto dir: directions) {
        simulate_move(grid, robot, dir);
        //std::cout << "State after move " << dir << std::endl;
        //print_actor_grid(grid);
    }

    long sum{0};
    for (auto actor: grid.actors()) {
        if (actor.type == BOX) {
            sum += 100 * actor.pos.second + actor.pos.first;
        }
    }

    return sum;
}

long part2(ActorGrid<WarehouseTiles, WarehouseActor>& grid, const std::vector<Direction>& directions) {
    WarehouseActor* robot = &*grid.find_actor_by([](WarehouseActor& actor) {return actor.type == ROBOT;});

    //std::cout << "Initial state" << std::endl;
    //print_actor_grid(grid);
    for (auto dir: directions) {
        enlarged_simulate_move(grid, robot, dir);
        //std::cout << "State after move " << dir << std::endl;
        //print_actor_grid(grid);
    }

    long sum{0};
    for (auto actor: grid.actors()) {
        if (actor.type == BOX_OPEN) {
            sum += 100 * actor.pos.second + actor.pos.first;
        }
    }

    return sum;
}

ActorGrid<WarehouseTiles, WarehouseActor> enlarge_grid(const ActorGrid<WarehouseTiles, WarehouseActor>& grid) {
    std::vector<WarehouseActor> enlarged_actors;
    std::vector<WarehouseTiles> enlarged_tiles;

    auto scale = [](Position pos) {
        return Position{pos.first * 2, pos.second};
    };

    for (auto it = grid.begin(); it != grid.end(); it++) {
        switch (*it) {
            case WALL:
                enlarged_tiles.push_back(WALL);
                enlarged_tiles.push_back(WALL);
                break;
            case EMPTY: 
                enlarged_tiles.push_back(EMPTY);
                enlarged_tiles.push_back(EMPTY);
                break;
            case ACTOR:
                Position pos = grid.to_grid_position(it);
                WarehouseActor actor = *grid.get_actor(pos);
                if (actor.type == ROBOT) {
                    enlarged_tiles.push_back(ACTOR);
                    enlarged_tiles.push_back(EMPTY);
                    actor.pos = scale(actor.pos);
                    enlarged_actors.push_back(actor);
                } else {
                    enlarged_tiles.push_back(ACTOR);
                    enlarged_tiles.push_back(ACTOR);
                    auto open_box = WarehouseActor{scale(actor.pos), actor.dir, BOX_OPEN};
                    auto close_box = WarehouseActor{scale(actor.pos), actor.dir, BOX_CLOSE};
                    close_box.pos.first++;
                    enlarged_actors.push_back(open_box);
                    enlarged_actors.push_back(close_box);
                }
                break;
        }
    }

    return ActorGrid{enlarged_tiles, enlarged_actors, grid.rows(), grid.cols() * 2, ACTOR};
}

int main() {
    std::string fileName;
    std::cin >> fileName;

    auto [grid, directions] = parse_input(fileName);
    auto large_grid = enlarge_grid(grid);
    long p1 = part1(grid, directions);
    std::cout << "Part 1: " << p1 << std::endl;

    long p2 = part2(large_grid, directions);
    std::cout << "Part 2: " << p2 << std::endl;
}