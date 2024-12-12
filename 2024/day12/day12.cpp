#include "../common/grid.hpp"
#include <iostream>
#include <fstream>
#include <set>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

using namespace grid;

Grid<char> parse_input(std::string fileName) {
    std::ifstream file{fileName};
    return Grid<char>::from(file, [](char c) {return c;});
}

std::vector<std::set<Position>> get_areas(Grid<char>& map) {
    std::vector<std::set<Position>> areas;

    for (auto it = map.begin(); it != map.end(); it++) {
        Position pos = map.to_grid_position(it);
        if (std::find_if(areas.begin(), areas.end(), [&pos](const std::set<Position>& area) {return area.find(pos) != area.end();}) == areas.end()) {
            std::set<Position> new_area = map.bfs(it, *it, [&map](const Position& current, const Position& neighbor) {return map.at(neighbor) == map.at(current);});
            areas.push_back(new_area);
        }
    }

    return areas;
}

long area(const std::set<Position>& plot) {
    return plot.size();
}

long perimeter(const Grid<char>& map, const std::set<Position>& area) {
    long p{0};

    for (const Position& pos: area) {
        std::vector<Position> neighbors = map.neighbors(pos);
        p += 4 - neighbors.size();
        for (const Position neighbor: map.neighbors(pos)) {
            if (map.at(neighbor) != map.at(pos)) {
                p++;
            }
        }
    }

    return p;
}

long part1(Grid<char>& map) {
    std::vector<std::set<Position>> areas = get_areas(map);
    return std::accumulate(areas.begin(), areas.end(), 0, [&map](long acc, const std::set<Position>& plot) {return acc + area(plot) * perimeter(map, plot);});
}

int main() {
    std::string fileName;
    std::cin >> fileName;

    Grid<char> map = parse_input(fileName);
    long p1 = part1(map);
    std::cout << "Part 1: " << p1 << std::endl;
}