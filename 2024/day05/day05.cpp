#include <iostream>
#include <fstream>
#include <map>
#include <set>
#include <vector>
#include <utility>
#include <string>
#include <algorithm>

using Page = int;
using PageOrder = std::map<Page, std::set<Page>>;
using Pages = std::vector<Page>;

bool less_than(const PageOrder& order, const Page& a, const Page& b) {
    return order.at(a).find(b) != order.at(a).end();
}

void add_page_keys(PageOrder& order) {
    std::set<Page> pages;
    for (const auto& [page, following] : order) {
        pages.insert(page);
        pages.insert(following.begin(), following.end());
    }

    for (const auto& page: pages) {
        order[page];
    }
}
    

PageOrder parse_page_order(std::istream& input) {
    PageOrder order;
    Page p1, p2;

    while (isdigit(input.peek())) {
        input >> p1;
        input.ignore(1); // Pipe
        input >> p2;
        input.ignore(1); // newline
    
        order[p1].insert(p2);
    }

    input.ignore(1); // newline

    return order;
}

std::vector<Pages> parse_pages(std::istream& input) {
    std::vector<Pages> page_lists;
    while (isdigit(input.peek())) {
        Pages pages;
        Page page;
        while (input.peek() != '\n' && input >> page) {
            pages.push_back(page);
            if (input.peek() == ',') {
                input.ignore(1);
            }
        }
        input.ignore(1); // newline

        page_lists.push_back(pages);
    }

   return page_lists;
}

std::pair<PageOrder, std::vector<Pages>> parse_input(std::string filename) {
    std::ifstream file{filename};
    PageOrder order = parse_page_order(file);
    std::vector<Pages> page_lists = parse_pages(file);
    add_page_keys(order);

    return {order, page_lists};
}

Page middle(const Pages& pages) {
    return pages[pages.size() / 2];
}

int part1(const PageOrder& order, const std::vector<Pages>& page_lists) {
    int count{0};
    for (const auto& pages: page_lists) {
        bool is_ordered{true};
        for (size_t i{0}; i < pages.size() - 1; i++) {
            if (less_than(order, pages[i + 1], pages[i])) {
                is_ordered = false;
                break;
            }
        }

        if (is_ordered) {
            count += middle(pages);
        }
    }

    return count;
}

int part2(const PageOrder& order, const std::vector<Pages>& page_lists) {
    std::vector<Pages> unordered;
    for (const auto& pages: page_lists) {
        for (size_t i{0}; i < pages.size() - 1; i++) {
            if (less_than(order, pages[i + 1], pages[i])) {
                unordered.push_back(pages);
                break;
            }
        }
    }

    int count{0};
    for (auto& pages: unordered) {
        std::sort(pages.begin(), pages.end(), [&order](const Page& a, const Page& b) {
            return less_than(order, a, b);
        });

        count += middle(pages);
    }

    return count;
}

int main() {
    std::string filename;
    std::cin >> filename;

    auto [order, page_lists] = parse_input(filename);
    int p1 = part1(order, page_lists);
    int p2 = part2(order, page_lists);
    std::cout << "Part 1: " << p1 << std::endl;
    std::cout << "Part 2: " << p2 << std::endl;
}