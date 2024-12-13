#include "../common/matrix.hpp"
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace matrix;

std::pair<long, long> parse_line(std::istream& is) {
    long x, y;

    while (!isdigit(is.get())) {}
    is.unget();

    is >> x;
    is.ignore(4);
    is >> y;

    return {x, y};
}

std::vector<std::pair<Matrix, Matrix>> parse_input(std::string filename) {
    std::ifstream file{filename};
    std::vector<std::pair<Matrix, Matrix>> matrices;

    while (file) {
        std::pair<long, long> xy1 = parse_line(file);
        std::pair<long, long> xy2 = parse_line(file);
        std::pair<long, long> targets = parse_line(file);
        file.get(); // newline
        file.get(); // skip newline, also invalidates file if at eof

        std::vector<std::vector<Fraction>> m = {{xy1.first, xy2.first}, {xy1.second, xy2.second}};
        std::vector<std::vector<Fraction>> t = {{targets.first}, {targets.second}};
        matrices.push_back({Matrix{m}, Matrix{t}});
    }

    return matrices;
}

long part1(std::vector<std::pair<Matrix, Matrix>> matrices) {
    long sum{0};

    for (auto& [m, t]: matrices) {
        try {
            Matrix result = m.inverse() * t;

            long x = result(0, 0).get();
            long y = result(1, 0).get();

            Matrix int_result = Matrix{{{x}, {y}}};

            if (0 < x && x <= 100 && 0 < y && y <= 100 && m * int_result == t.transpose()) {
                sum += 3*x + y;
            }
        } catch (Matrix::MatrixArithmeticError& e) {
            std::cout << e.what() << std::endl;
        }
    }

    return sum;
}

long part2(std::vector<std::pair<Matrix, Matrix>> matrices) {
    long sum{0};

    for (auto& [m, t]: matrices) {
        try {
            Matrix scaled_t = t + Matrix{{{10000000000000}, {10000000000000}}};
            Matrix result = m.inverse() * scaled_t;

            long x = result(0, 0).get();
            long y = result(1, 0).get();

            Matrix int_result = Matrix{{{x}, {y}}};

            if (m * int_result == scaled_t.transpose()) {
                sum += 3*x + y;
            }
        } catch (Matrix::MatrixArithmeticError& e) {
            std::cout << e.what() << std::endl;
        }
    }

    return sum;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto matrices = parse_input(filename);
    long p1 = part1(matrices);
    std::cout << "Part 1: " << p1 << std::endl;
    long p2 = part2(matrices);
    std::cout << "Part 2: " << p2 << std::endl;
}