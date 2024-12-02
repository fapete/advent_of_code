#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

struct NumLists {
    std::vector<long> left;
    std::vector<long> right;

    void sort() {
        std::sort(left.begin(), left.end());
        std::sort(right.begin(), right.end());
    }
};

NumLists parse_input(std::string filename) {
    std::ifstream file{filename};
    
    std::vector<long> left;
    std::vector<long> right;

    long l, r;

    while (file >> l >> r) {
        left.push_back(l);
        right.push_back(r);
    }

    return NumLists{left, right};
}

long part1(const NumLists& nums) {
    std::vector<long> diffs;
    std::transform(nums.left.begin(), nums.left.end(), nums.right.begin(), std::back_inserter(diffs), [](const long& l, const long& r) {return std::abs(l - r);});
    return std::accumulate(diffs.begin(), diffs.end(), 0);
}

long part2(const NumLists& nums) {
    long result{0};

    auto lPos = nums.left.begin();
    auto rPos = nums.right.begin();

    while (lPos != nums.left.end()) {
        while (*rPos < *lPos) {rPos++;}

        long rCount{0};
        long lCount{0};
        long l = *lPos;

        while (rPos != nums.right.end() && *rPos == *lPos) {
            rCount++;
            rPos++;
        }

        while (lPos != nums.left.end() && *lPos == l) {
            lCount++;
            lPos++;
        }

        result += lCount * l * rCount;
    }

    return result;
}

int main() {
    std::string filename;
    std::cin >> filename;

    NumLists nums = parse_input(filename);
    nums.sort();

    long p1 = part1(nums);
    long p2 = part2(nums);
    std::cout << "Part 1: " << p1 << std::endl;
    std::cout << "Part 2: " << p2 << std::endl;

    return 0;
}