#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <vector>

std::vector<std::string> read_lines(const std::string & filename)
{
    std::fstream File;
    File.open(filename);
    if (!File) {
        std::cerr << "No file with such name!" << std::endl;
        exit(0);
    }
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(File, line)) {
        lines.push_back(line);
    }

    return lines;
}

int main(int argc, char * argv[])
{
    std::vector<std::string> args(argv, argv + argc);

    static const std::string short_range = "-i";
    static const std::string long_range = "--input-range=";
    static const std::string short_lines = "-n";
    static const std::string long_lines = "--head-count=";
    static const std::string short_repeat = "-r";
    static const std::string long_repeat = "--repeat";

    int max_lines = -1;
    int min = 0;
    int max = 0;
    int repeat = 0;
    int file = 1;

    for (std::size_t i = 0; i < args.size(); i++) {
        if (args[i] == short_lines) {
            max_lines = stoi(args[i + 1]);
        }
        if (args[i] == short_repeat || args[i] == long_repeat) {
            repeat = 1;
        }
        if (args[i] == short_range) {
            std::string numbers = args[i + 1];
            int hyphen = numbers.find("-");
            min = stoi(numbers.substr(0, hyphen));
            max = stoi(numbers.substr(hyphen + 1, numbers.size() - hyphen - 1));
            file = 0;
        }
        if (args[i].find(long_range) != std::string::npos) {
            int len = args[i].size() - long_range.size() + 1;
            std::string numbers = args[i].substr(long_range.size(), len);
            int hyphen = numbers.find("-");
            min = stoi(numbers.substr(0, hyphen));
            max = stoi(numbers.substr(hyphen + 1, numbers.size() - hyphen - 1));
            file = 0;
        }
        if (args[i].find(long_lines) != std::string::npos) {
            int len = args[i].size() - long_lines.size();
            max_lines = stoi(args[i].substr(long_lines.size(), len));
        }
    }

    if (file) {
        std::vector<std::string> lines = read_lines(args[args.size() - 1]);
        if (repeat) {
            if (max_lines == -1) {
                while (true) {
                    std::cout << lines[rand() % lines.size()] << std::endl;
                }
            }
            else {
                for (int i = 0; i < max_lines; i++) {
                    std::cout << lines[rand() % lines.size()] << std::endl;
                }
                exit(0);
            }
        }
        else {
            std::random_shuffle(lines.begin(), lines.end());
            if (max_lines == -1 || max_lines > static_cast<int>(lines.size())) {
                max_lines = lines.size();
            }
            for (int i = 0; i < max_lines; i++) {
                std::cout << lines[i] << std::endl;
            }
            exit(0);
        }
    }
    else {
        if (repeat) {
            if (max_lines == -1) {
                while (true) {
                    std::cout << min + (rand() % (max - min + 1)) << std::endl;
                }
            }
            else {
                for (int i = 0; i < max_lines; i++) {
                    std::cout << min + (rand() % (max - min + 1)) << std::endl;
                }
                exit(0);
            }
        }
        else {
            std::vector<int> numbers(max - min + 1);
            std::iota(std::begin(numbers), std::end(numbers), min);
            std::random_shuffle(numbers.begin(), numbers.end());
            if (max_lines == -1 || max_lines > static_cast<int>(numbers.size())) {
                max_lines = numbers.size();
            }
            for (int i = 0; i < max_lines; i++) {
                std::cout << numbers[i] << std::endl;
            }
            exit(0);
        }
    }
}