#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

long to_trinary(long n) {
    long result{0};
    long base{1};

    while (n > 0) {
        result += (n % 3) * base;
        n /= 3;
        base *= 10;
    }

    return result;
}

struct Equation {
    long target;
    std::vector<long> numbers;

    bool can_be_true() const {
        int is_mult = 0;

        while (is_mult < pow(2, numbers.size() - 1)) {
            long result = numbers[0];
            for (int i = 0; i < numbers.size() - 1; i++) {
                if (is_mult & (1 << i)) {
                    result *= numbers[i + 1];
                }
                else {
                    result += numbers[i + 1];
                }
            }

            if (result == target) {
                return true;
            }
            is_mult++;
        }

        return false;
    }

    bool can_be_true_with_concat() const {
        long is_mult = 0;

        while (is_mult < pow(3, numbers.size() - 1)) {
            long result = numbers[0];
            long trinary = to_trinary(is_mult);
            for (int i = 0; i < numbers.size() - 1; i++) {
                if (trinary % 10 == 0) {
                    result += numbers[i + 1];
                }
                else if (trinary % 10 == 1) {
                    result *= numbers[i + 1];
                }
                else {
                    result = result * (pow(10, floor(log10(numbers[i + 1])) + 1)) + numbers[i + 1];
                }
                trinary /= 10;
            }

            if (result == target) {
                return true;
            }
            is_mult++;
        }

        return false;
    }

    friend std::istream& operator>>(std::istream& is, Equation& eq);
    friend std::ostream& operator<<(std::ostream& os, const Equation& eq);
};

std::istream& operator>>(std::istream& is, Equation& eq) {
    is >> eq.target;

    is.ignore(2);

    while (is && is.peek() != '\n') {
        long num;
        is >> num;
        eq.numbers.push_back(num);
    }

    return is;
}

std::ostream& operator<<(std::ostream& os, const Equation& eq) {
    os << eq.target << ": ";
    for (auto num: eq.numbers) {
        os << num << " ";
    }
    return os;
}

std::vector<Equation> parse_input(std::string filename) {
    std::ifstream file{filename};
    std::vector<Equation> equations;

    while (file) {
        Equation eq;
        file >> eq;
        if (file) {
            equations.push_back(eq);
        }
    }

    return equations;
}

long part1(const std::vector<Equation>& equations) {
    long sum{0};

    for (auto& eq: equations) {
        if (eq.can_be_true()) {
            sum += eq.target;
        }
    }
    return sum;
}

long part2(const std::vector<Equation>& equations) {
    long sum{0};

    for (auto& eq: equations) {
        if (eq.can_be_true() || eq.can_be_true_with_concat()) {
            sum += eq.target;
        }
    }
    return sum;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto equations = parse_input(filename);

    long p1 = part1(equations);
    std::cout << "Part 1: " << p1 << std::endl;
    long p2 = part2(equations);
    std::cout << "Part 2: " << p2 << std::endl;

    return 0;
}