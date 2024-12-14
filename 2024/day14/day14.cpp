#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
#include <string>
#include <set>

long euclidean_mod(long x, long m)
{
    return (x % m + m) % m;
}

struct Robot
{
    std::pair<long, long> pos;
    std::pair<long, long> dir;

    friend std::ostream &operator<<(std::ostream &os, const Robot &r);
};

std::ostream &operator<<(std::ostream &os, const Robot &r)
{
    os << "Pos: (" << r.pos.first << ", " << r.pos.second << "), Dir: (" << r.dir.first << ", " << r.dir.second << ")";

    return os;
}

std::vector<Robot> parse_input(std::string filename)
{
    std::ifstream file{filename};
    std::vector<Robot> robots;

    while (file)
    {
        long x_pos, y_pos, x_dir, y_dir;
        while (!isdigit(file.get()))
        {
        }
        file.unget();

        file >> x_pos;
        file.ignore(1);
        file >> y_pos;
        file.ignore(3);
        file >> x_dir;
        file.ignore(1);
        file >> y_dir;
        file.get(); // newline + invalidates at eof
        file.get(); // newline + invalidates at eof

        Robot r{{x_pos, y_pos}, {x_dir, y_dir}};
        robots.push_back(r);
    }

    return robots;
}

Robot move_robot(const Robot &r, long steps, int x_dim, int y_dim)
{
    long new_x = euclidean_mod(r.pos.first + r.dir.first * steps, x_dim);
    long new_y = euclidean_mod(r.pos.second + r.dir.second * steps, y_dim);

    return Robot{{new_x, new_y}, r.dir};
}

std::vector<Robot> move_robots(std::vector<Robot> &robots, long steps, int x_dim, int y_dim)
{
    for (auto it = robots.begin(); it != robots.end(); it++)
    {
        *it = move_robot(*it, steps, x_dim, y_dim);
    }

    return robots;
}

int is_in_quadrant(const Robot &r, long x_dim, long y_dim)
{
    if (r.pos.first < x_dim / 2)
    {
        if (r.pos.second < y_dim / 2)
            return 1;
        else if (r.pos.second > y_dim / 2)
            return 3;
    }
    else if (r.pos.first > x_dim / 2)
    {
        if (r.pos.second < y_dim / 2)
            return 2;
        else if (r.pos.second > y_dim / 2)
            return 4;
    }

    return 0;
}

long part1(std::vector<Robot> robots, long seconds, long x_dim, long y_dim)
{
    robots = move_robots(robots, seconds, x_dim, y_dim);

    long q1_count{0};
    long q2_count{0};
    long q3_count{0};
    long q4_count{0};
    for (const auto &robot : robots)
    {
        switch (is_in_quadrant(robot, x_dim, y_dim))
        {
        case 1:
            q1_count++;
            break;
        case 2:
            q2_count++;
            break;
        case 3:
            q3_count++;
            break;
        case 4:
            q4_count++;
            break;
        default:
            break;
        }
    }

    return q1_count * q2_count * q3_count * q4_count;
}

void print_robots(const std::vector<Robot> &robots, long x_dim, long y_dim)
{
    for (long x = 0; x < x_dim; x++)
    {
        for (long y = 0; y < y_dim; y++)
        {
            int count = std::count_if(robots.begin(), robots.end(), [x, y](const auto &r)
                                      { return r.pos.first == x && r.pos.second == y; });
            std::cout << (count > 0 ? '#' : '.');
        }
        std::cout << std::endl;
    }
}

long part2(std::vector<Robot> robots, long x_dim, long y_dim)
{
    int seconds{1};
    while (true)
    {
        std::set<std::pair<long, long>> unique_positions;
        robots = move_robots(robots, 1, x_dim, y_dim);
        for (auto r : robots)
        {
            unique_positions.insert(r.pos);
        }
        if (unique_positions.size() == robots.size())
        {
            print_robots(robots, x_dim, y_dim);
            return seconds;
        }
        seconds++;
    }
}

int main()
{
    std::string file;
    long seconds{100}, x_dim{101}, y_dim{103};

    std::cin >> file;

    auto robots = parse_input(file);

    long p1 = part1(robots, seconds, x_dim, y_dim);
    std::cout << "Part 1: " << p1 << std::endl;

    robots = parse_input(file);
    long p2 = part2(robots, x_dim, y_dim);
    std::cout << "Part 2: " << p2 << std::endl;

    return 0;
}