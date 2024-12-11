#include "../common/grid.hpp"
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace grid;

int parse_tile(char c) {
    return c - '0';
}

Grid<int> parse_input(std::string fileName) {
    std::ifstream file{fileName};
    return Grid<int>::from(file, parse_tile);
}

int part1(Grid<int>& grid) {
    int sum{0};

    for (auto i = grid.begin(); i != grid.end(); i++) {
        if (*i == 0) {
            sum += grid.bfs(i, 9, [&grid](const Position& current, const Position& neighbor) {return grid.at(neighbor) - grid.at(current) == 1;}).size();
        }
    }

    return sum;
}

int part2(Grid<int>& grid) {
    int sum{0};

    for (auto i = grid.begin(); i != grid.end(); i++) {
        if (*i == 0) {
            sum += grid.dfs(i, 9, [&grid](const Position& current, const Position& neighbor) {return grid.at(neighbor) - grid.at(current) == 1;}, true).size();
        }
    }

    return sum;
}

int main() {
    std::string fileName;
    std::cin >> fileName;

    Grid<int> grid = parse_input(fileName);
    int p1 = part1(grid);
    std::cout << "Part 1: " << p1 << std::endl;
    int p2 = part2(grid);
    std::cout << "Part 2: " << p2 << std::endl;
}