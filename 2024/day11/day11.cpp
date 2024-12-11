#include <iostream>
#include <fstream>
#include <unordered_map>
#include <string>

std::unordered_map<long, long> parse_input(std::string filename) {
    std::ifstream file{filename};
    std::unordered_map<long, long> pebbles;

    long num;

    while (file >> num) {
        pebbles[num]++;
    }

    return pebbles;
}

std::unordered_map<long, long> simulation_step(std::unordered_map<long, long>& pebbles) {
    std::unordered_map<long, long> new_pebbles;

    for (auto& [num, count]: pebbles) {
        if (num == 0) {
            new_pebbles[1] += count;
        } else if ((long)(std::floor(std::log10(num)) + 1) % 2 == 0) {
            long num_digits = std::floor(std::log10(num)) + 1;
            long left = num / std::pow(10, num_digits / 2);
            long right = num % (long)std::pow(10, num_digits / 2);
            new_pebbles[left] += count;
            new_pebbles[right] += count;
        } else {
            new_pebbles[num * 2024] += count;
        }
    }

    return new_pebbles;
}

long simulate_for(std::unordered_map<long, long> pebbles, int steps) {
    for (int i = 0; i < steps; i++) {
        pebbles = simulation_step(pebbles);
    }

    long count{0};
    for (auto& [_, pCount]: pebbles) {
        count += pCount;
    }

    return count;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto pebbles = parse_input(filename);

    long p1 = simulate_for(pebbles, 25);
    std::cout << "Part 1: " << p1 << std::endl;
    long p2 = simulate_for(pebbles, 75);
    std::cout << "Part 2: " << p2 << std::endl;

    return 0;
}