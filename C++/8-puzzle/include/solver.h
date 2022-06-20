#pragma once

#include "board.h"

class Solver
{
    class Solution
    {
    public:
        Solution() = default;

        explicit Solution(const std::vector<Board> & initial);

        std::size_t moves() const
        {
            if (m_moves.size() <= 1) {
                return 0;
            }
            else {
                return m_moves.size() - 1;
            }
        }

        using const_iterator = std::vector<Board>::const_iterator;

        const_iterator begin() const { return m_moves.begin(); }

        const_iterator end() const { return m_moves.end(); }

    private:
        std::vector<Board> m_moves;
    };

public:
    static Solution solve(const Board & initial);
};
