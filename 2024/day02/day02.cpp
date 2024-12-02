#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>
#include <sstream>

using Report = std::vector<long>;

std::vector<Report> parse_input(std::string filename) {
    std::ifstream file{filename};
    
    std::vector<Report> reports;
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream iss{line};
        Report report;
        long number;
        while (iss >> number) {
            report.push_back(number);
        }
        reports.push_back(report);
    }

    return reports;
}

void print_report(const Report& report) {
    for (const auto& n : report) {
        std::cout << n << " ";
    }
    std::cout << std::endl;
}

bool is_safe(const Report& report) {
    std::vector<long> diffs;
    std::adjacent_difference(report.begin(), report.end(), std::back_inserter(diffs), [](const long& a, const long& b) {return a - b;});
    return std::all_of(diffs.begin() + 1, diffs.end(), [](const long& n) {return n > 0 && n < 4;}) || std::all_of(diffs.begin() + 1, diffs.end(), [](const long& n) {return n < 0 && n > -4;});
}

Report copy_all_except(const Report& report, Report::size_type index) {
    Report copy;
    for (auto i = 0; i < report.size(); i++) {
        if (i != index) {
            copy.push_back(report[i]);
        }
    }

    return copy;
}

bool is_safe_with_dampener(const Report& report) {
    if (is_safe(report)) {
        return true;
    }

    for (auto i = 0; i < report.size(); i++) {
        auto copy = copy_all_except(report, i);
        if (is_safe(copy)) {
            return true;
        }
    }

    return false;
}

long part1(const std::vector<Report>& reports) {
    return std::count_if(reports.begin(), reports.end(), is_safe);
}

long part2(const std::vector<Report>& reports) {
    return std::count_if(reports.begin(), reports.end(), is_safe_with_dampener);
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto reports = parse_input(filename);
    long p1 = part1(reports);
    std::cout << "Part 1: " << p1 << std::endl;
    long p2 = part2(reports);
    std::cout << "Part 2: " << p2 << std::endl;
}