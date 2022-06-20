#include "solver.h"

#include <algorithm>
#include <queue>
#include <unordered_map>
#include <unordered_set>

Solver::Solution::Solution(const std::vector<Board> & moves)
    : m_moves(moves)
{
}

Solver::Solution Solver::solve(const Board & board)
{
    Board tmp = Board(std::vector<std::vector<unsigned>>(board.size(), std::vector<unsigned>(board.size())));
    if (board.size() <= 1) {
        return Solver::Solution({tmp});
    }
    if (!board.is_solvable()) {
        return Solver::Solution();
    }

    std::priority_queue<Board> q;
    std::unordered_set<Board, HashBoard> visited;

    q.push(board);

    std::unordered_map<Board, Board, HashBoard> ancestors;
    ancestors.insert({board, board});

    while (!q.empty()) {
        Board now = q.top();

        q.pop();
        visited.insert(now);

        if (now.is_goal()) {
            std::vector<Board> path;
            while (now != board) {
                path.emplace_back(now);
                now = ancestors[now];
            }
            path.emplace_back(board);
            std::reverse(path.begin(), path.end());
            return Solution(path);
        }

        for (Board & near : Board::nearby(now)) {
            if (visited.count(near) == 0) {
                ancestors[near] = now;
                near.update_from_start(now);
                q.push(near);
            }
            else if (now.possible() < near.from_start()) {
                ancestors[near] = now;
                near.update_from_start(now);
            }
        }
    }

    return Solver::Solution({tmp});
}
