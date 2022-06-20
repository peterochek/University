#include "solver.h"

#include <chrono>
#include <iostream>

int main()
{
    const auto board = Board({{1, 4, 13, 11, 5},
                              {19, 6, 14, 10, 15},
                              {8, 16, 17, 0, 24},
                              {9, 7, 21, 20, 12},
                              {3, 18, 22, 23, 2}});

    auto start = std::chrono::high_resolution_clock::now();
    const auto solution = Solver::solve(board);
    auto stop = std::chrono::high_resolution_clock::now();

    auto duration = stop - start;

    double time = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();

    std::cout << time / 1000 << " seconds" << std::endl;
}
