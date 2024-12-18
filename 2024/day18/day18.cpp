#include "../common/grid.hpp"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace grid;

std::vector<Position> parse_input(std::string filename) {
    std::ifstream file{filename};
    std::vector<Position> positions;
    
    int x, y;
    while (file) {
        file >> x;
        file.ignore(1);
        file >> y;
        positions.push_back({x, y});
        file.get(); // ignore newline
    }

    return positions;
}

enum Tiles {EMPTY, CORRUPT, END};

std::ostream& operator<<(std::ostream& os, const Tiles& t) {
    switch (t) {
        case EMPTY:
            os << ".";
            break;
        case CORRUPT:
            os << "#";
            break;
        case END:
            os << "E";
            break;
    }

    return os;
}

void print_grid(const Grid<Tiles>& grid) {
    for (int y = 0; y < grid.rows(); y++) {
        for (int x = 0; x < grid.cols(); x++) {
            Position pos{x, y};
            std::cout << grid.at(pos);
        }
        std::cout << std::endl;
    }
}

int part1(const std::vector<Position>& positions, int dimensions, int bytesFall) {
    Grid<Tiles> grid{std::vector<Tiles>(dimensions * dimensions, EMPTY), dimensions, dimensions};

    for (int i = 0; i < bytesFall; i++) {
        grid.set_tile_at(positions[i], CORRUPT);
    }

    grid.set_tile_at({dimensions-1, dimensions-1}, END);

    auto path = grid.bfs(grid.begin(), END, [&grid](Position, Position p) {return grid.at(p) != CORRUPT;}, true);

    return path.begin()->second;
}

Position part2(const std::vector<Position>& positions) {
    Grid<Tiles> grid{std::vector<Tiles>(71 * 71, EMPTY), 71, 71};

    for (int i = 0; i < 1024; i++) {
        grid.set_tile_at(positions[i], CORRUPT);
    }
    grid.set_tile_at({70, 70}, END);

    size_t idx = 1024;
    while (!grid.bfs(grid.begin(), END, [&grid](Position, Position p) {return grid.at(p) != CORRUPT;}, true).empty()) {
        grid.set_tile_at(positions[idx], CORRUPT);
        idx++;
    }

    return positions[idx-1];
}

int main() {
    std::string filename;
    std::cin >> filename;

    std::vector<Position> positions = parse_input(filename);

    int p1 = part1(positions, 71, 1024); 
    std::cout << "Part 1: " << p1 << std::endl;

    Position p2 = part2(positions); 
    std::cout << "Part 2: " << p2 << std::endl;
    return 0;
}