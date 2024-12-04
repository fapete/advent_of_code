#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int count_around_for_xmas(const std::vector<std::string>& char_matrix, int row, int col) {
    int count{0};
    if (char_matrix[row][col] != 'X') return count;

    if (col < char_matrix[row].size() - 3) {
        if (char_matrix[row][col + 1] == 'M' && char_matrix[row][col + 2] == 'A' && char_matrix[row][col + 3] == 'S') {
            count++;
        }
    }
    if (row < char_matrix.size() - 3) {
        if (char_matrix[row + 1][col] == 'M' && char_matrix[row + 2][col] == 'A' && char_matrix[row + 3][col] == 'S') {
            count++;
        }
    }
    if (col < char_matrix[row].size() - 3 && row < char_matrix.size() - 3) {
        if (char_matrix[row + 1][col + 1] == 'M' && char_matrix[row + 2][col + 2] == 'A' && char_matrix[row + 3][col + 3] == 'S') {
            count++;
        }
    }
    if (col < char_matrix[row].size() - 3 && row > 2) {
        if (char_matrix[row - 1][col + 1] == 'M' && char_matrix[row - 2][col + 2] == 'A' && char_matrix[row - 3][col + 3] == 'S') {
            count++;
        }
    }
    if (col > 2) {
        if (char_matrix[row][col - 1] == 'M' && char_matrix[row][col - 2] == 'A' && char_matrix[row][col - 3] == 'S') {
            count++;
        }
    }
    if (row > 2) {
        if (char_matrix[row - 1][col] == 'M' && char_matrix[row - 2][col] == 'A' && char_matrix[row - 3][col] == 'S') {
            count++;
        }
    }
    if (col > 2 && row > 2) {
        if (char_matrix[row - 1][col - 1] == 'M' && char_matrix[row - 2][col - 2] == 'A' && char_matrix[row - 3][col - 3] == 'S') {
            count++;
        }
    }
    if (col > 2 && row < char_matrix.size() - 3) {
        if (char_matrix[row + 1][col - 1] == 'M' && char_matrix[row + 2][col - 2] == 'A' && char_matrix[row + 3][col - 3] == 'S') {
            count++;
        }
    }

    return count;
}

int count_around_for_mas_x(const std::vector<std::string>& char_matrix, int row, int col) {
    int count{0};
    if (char_matrix[row][col] != 'A') return count;

    if (col > 0 && row > 0 && col < char_matrix[row].size() - 1 && row < char_matrix.size() - 1) {
        if ((
                (char_matrix[row - 1][col - 1] == 'M' && char_matrix[row + 1][col + 1] == 'S') ||
                (char_matrix[row - 1][col - 1] == 'S' && char_matrix[row + 1][col + 1] == 'M')
            ) && (
                (char_matrix[row + 1][col - 1] == 'M' && char_matrix[row - 1][col + 1] == 'S') ||
                (char_matrix[row + 1][col - 1] == 'S' && char_matrix[row - 1][col + 1] == 'M')
            )
        ) {
            count++;
        }
    }

    return count;
}

std::vector<std::string> parse_input(std::string filename) {
    std::ifstream file{filename};

    std::vector<std::string> char_matrix;
    std::string line;
    while (file >> line) {
        char_matrix.push_back(line);
    }

    return char_matrix;
}

int part1(const std::vector<std::string>& char_matrix) {
    int count{0};
    for (int row = 0; row < char_matrix.size(); row++) {
        for (int col = 0; col < char_matrix[row].size(); col++) {
            count += count_around_for_xmas(char_matrix, row, col);
        }
    }

    return count;
}

int part2(const std::vector<std::string>& char_matrix) {
    int count{0};
    for (int row = 0; row < char_matrix.size(); row++) {
        for (int col = 0; col < char_matrix[row].size(); col++) {
            count += count_around_for_mas_x(char_matrix, row, col);
        }
    }

    return count;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto char_matrix = parse_input(filename);
    std::cout << "Part 1: " << part1(char_matrix) << std::endl;
    std::cout << "Part 2: " << part2(char_matrix) << std::endl;

    return 0;
}