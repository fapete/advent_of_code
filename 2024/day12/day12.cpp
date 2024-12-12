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

bool is_outer_corner(const Grid<char>& map, const Position& pos, const Position& adjacent1, const Position& adjacent2) {
    return (!map.is_in_bounds(adjacent1) || map.at(adjacent1) != map.at(pos)) && (!map.is_in_bounds(adjacent2) || map.at(adjacent2) != map.at(pos));
}

bool is_inner_corner(const Grid<char>& map, const Position& pos, const Position& adjacent1, const Position& adjacent2, const Position& diag) {
    return (map.is_in_bounds(adjacent1) && map.is_in_bounds(adjacent2) && map.is_in_bounds(diag)) && 
        (map.at(adjacent1) != map.at(pos) && map.at(adjacent2) == map.at(pos) && map.at(diag) == map.at(pos));
}

long count_corners(const Grid<char>& map, const Position& pos) {
    long count{0};

    // Outer Corners
    Position above = map.get_position_in_direction(pos, UP);
    Position left = map.get_position_in_direction(pos, LEFT);
    Position below = map.get_position_in_direction(pos, DOWN);
    Position right = map.get_position_in_direction(pos, RIGHT);

    if (is_outer_corner(map, pos, above, left)) count++;
    if (is_outer_corner(map, pos, above, right)) count++;
    if (is_outer_corner(map, pos, below, left)) count++;
    if (is_outer_corner(map, pos, below, right)) count++;

    // Inner Corners
    Position above_left = map.get_position_in_direction(above, LEFT);
    Position above_right = map.get_position_in_direction(above, RIGHT);
    Position below_left = map.get_position_in_direction(below, LEFT);
    Position below_right = map.get_position_in_direction(below, RIGHT);

    if (is_inner_corner(map, pos, right, above, above_right)) count++;
    if (is_inner_corner(map, pos, left, above, above_left)) count++;
    if (is_inner_corner(map, pos, right, below, below_right)) count++;
    if (is_inner_corner(map, pos, left, below, below_left)) count++;

    return count;
}

long count_all_corners(const Grid<char>& map, const std::set<Position>& area) {
    long count{0};

    for (const Position& pos: area) {
        count += count_corners(map, pos);
    }

    return count;
}

long part1(Grid<char>& map) {
    std::vector<std::set<Position>> areas = get_areas(map);
    return std::accumulate(areas.begin(), areas.end(), 0, [&map](long acc, const std::set<Position>& plot) {return acc + area(plot) * perimeter(map, plot);});
}

long part2(Grid<char>& map) {
    std::vector<std::set<Position>> areas = get_areas(map);
    return std::accumulate(areas.begin(), areas.end(), 0, [&map](long acc, const std::set<Position>& plot) {return acc + area(plot) * count_all_corners(map, plot);});
}

int main() {
    std::string fileName;
    std::cin >> fileName;

    Grid<char> map = parse_input(fileName);
    long p1 = part1(map);
    std::cout << "Part 1: " << p1 << std::endl;
    long p2 = part2(map);
    std::cout << "Part 2: " << p2 << std::endl;

    return 0;
}