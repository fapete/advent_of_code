#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <numeric>
#include <string>

struct Mul {
    long x;
    long y;

    long operator()() const {
        return x * y;
    }
};

long part1(std::vector<Mul> muls) {
    return std::accumulate(muls.begin(), muls.end(), 0, [](long acc, const Mul& m) {return acc + m();});
}

std::vector<Mul> parse_input1(std::string filename) {
    std::ifstream file{filename};

    std::vector<Mul> muls;
    std::regex mul{R"(mul\((\d{1,3}),(\d{1,3})\))"};
    std::smatch matches;

    std::string input;
    while (file >> input) {
        std::string::const_iterator searchStart(input.cbegin());
        while (std::regex_search(searchStart, input.cend(), matches, mul)) {
            muls.push_back(Mul{std::stol(matches[1]), std::stol(matches[2])});
            searchStart = matches.suffix().first;
        }
    }
    
    return muls;
}

std::vector<Mul> parse_input2(std::string filename) {
    std::ifstream file{filename};

    std::vector<Mul> muls;
    std::regex mul{R"((do\(\))|(don't\(\))|(mul\((\d{1,3}),(\d{1,3})\)))"};
    std::smatch matches;

    bool enabled = true;

    std::string input;
    while (file >> input) {
        std::string::const_iterator searchStart(input.cbegin());
        while (std::regex_search(searchStart, input.cend(), matches, mul)) {
            if (enabled && matches[1] == "" && matches[2] == "") {
                muls.push_back(Mul{std::stol(matches[4]), std::stol(matches[5])});
            }
            if (matches[2] != "") {
                enabled = false;
            }
            if (matches[1] != "") {
                enabled = true;
            }
            searchStart = matches.suffix().first;
        }
    }
    
    return muls;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto muls = parse_input1(filename);
    std::cout << part1(muls) << std::endl;
    auto muls2 = parse_input2(filename);
    std::cout << part1(muls2) << std::endl;
}