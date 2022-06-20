#include "board.h"

#include <algorithm>
#include <random>

Board::Board(const std::vector<std::vector<unsigned>> & data)
    : data(data)
    , hash(Board::hasher(*this))
{
}

Board Board::create_random(const unsigned size)
{
    std::vector<unsigned> rnd(size * size);
    std::iota(std::begin(rnd), std::end(rnd), 0);
    std::shuffle(rnd.begin(), rnd.end(), std::mt19937(std::random_device()()));

    std::vector<std::vector<unsigned>> board(size, std::vector<unsigned>(size));

    for (std::size_t i = 0; i < size; ++i) {
        for (std::size_t j = 0; j < size; ++j) {
            board[i][j] = rnd[size * i + j];
        }
    }

    return Board(board);
}

std::size_t Board::size() const
{
    return data.size();
}

unsigned Board::hamming() const
{
    unsigned hamming = 0;
    unsigned expected = 0;
    const unsigned max = size() > 0 ? size() * size() - 1 : 0;
    for (const auto & row : data) {
        for (const auto num : row) {
            if (expected < max) {
                ++expected;
            }
            else {
                expected = 0;
            }
            if (num != expected) {
                ++hamming;
            }
        }
    }

    return hamming;
}

unsigned Board::manhattan() const
{
    std::size_t diff = 0;
    for (std::size_t i = 0; i < size(); ++i) {
        for (std::size_t j = 0; j < size(); ++j) {
            if (data[i][j] != 0) {
                std::size_t row = (data[i][j] - 1) / size();
                std::size_t col = (data[i][j] - 1) % size();

                diff += std::max(i, row) - std::min(i, row) +
                        std::max(j, col) - std::min(j, col);
            }
        }
    }

    return diff;
}

bool Board::is_goal() const
{
    return hamming() == 0;
}

bool Board::is_solvable() const
{
    if (size() <= 1 || is_goal()) {
        return true;
    }

    std::size_t inv = inversions();
    if (size() % 2 == 1) {
        return inv % 2 == 0;
    }
    else {
        auto row = gap().first;
        bool even = (size() - row) % 2 == 0;
        if (even && inv % 2 == 1) {
            return true;
        }
        if (!even && inv % 2 == 0) {
            return true;
        }
        return false;
    }
}

std::string Board::to_string() const
{
    std::string repr;
    repr.reserve(size() * (2 * size() + 1));
    for (std::size_t i = 0; i < size(); ++i) {
        for (std::size_t j = 0; j < size(); ++j) {
            if (j != 0) {
                repr += ' ';
            }

            repr += std::to_string(data[i][j]);
        }

        repr += '\n';
    }

    return repr;
}

std::size_t Board::heuristics() const
{
    return to_end() + from_start();
}

std::size_t Board::from_start() const
{
    return moves;
}

void Board::update_from_start(const Board & better)
{
    moves = better.from_start() + 1;
}

std::size_t Board::to_end() const
{
    return 3.03 * manhattan();
}

std::size_t Board::possible() const
{
    return from_start() + 1;
}

std::vector<Board> Board::nearby(const Board & current)
{
    std::vector<Board> nearby;
    std::pair<std::size_t, std::size_t> gap = current.gap();
    auto tmp = current.data;

    auto swapper = [&gap, &nearby, &tmp](std::size_t dy, std::size_t dx) {
        std::swap(tmp[gap.first][gap.second], tmp[gap.first + dy][gap.second + dx]);
        nearby.emplace_back(tmp);
        std::swap(tmp[gap.first][gap.second], tmp[gap.first + dy][gap.second + dx]);
    };

    if (gap.first != current.size() - 1) {
        swapper(1, 0);
    }
    if (gap.first != 0) {
        swapper(-1, 0);
    }
    if (gap.second != current.size() - 1) {
        swapper(0, 1);
    }
    if (gap.second != 0) {
        swapper(0, -1);
    }

    return nearby;
}

std::size_t Board::get_hash() const
{
    return hash;
}

bool operator<(const Board & a, const Board & b)
{
    return a.heuristics() > b.heuristics();
}

bool operator==(const Board & lhs, const Board & rhs)
{
    return lhs.data == rhs.data;
}

bool operator!=(const Board & lhs, const Board & rhs)
{
    if (lhs.get_hash() != rhs.get_hash()) {
        return true;
    }
    else {
        return !(lhs == rhs);
    }
}

std::size_t Board::inversions() const
{
    std::size_t counter = 0;
    for (std::size_t i = 0; i < size() * size(); i++) {
        std::size_t row_outer = i / size();
        std::size_t col_outer = i % size();
        for (std::size_t j = i + 1; j < size() * size(); ++j) {
            std::size_t row_inner = j / size();
            std::size_t col_inner = j % size();
            if (data[row_outer][col_outer] != 0 && data[row_inner][col_inner] != 0) {
                if (data[row_outer][col_outer] > data[row_inner][col_inner]) {
                    counter++;
                }
            }
        }
    }

    return counter;
}

std::pair<std::size_t, std::size_t> Board::gap() const
{
    for (std::size_t i = 0; i < size(); ++i) {
        for (std::size_t j = 0; j < size(); ++j) {
            if (data[i][j] == 0) {
                return std::make_pair(i, j);
            }
        }
    }

    return std::make_pair(0, 0);
}

std::size_t Board::hasher(Board & init)
{
    init.hash = init.size();
    for (const auto & row : init.data) {
        for (const auto num : row) {
            init.hash = 31 * init.hash + num;
        }
    }

    return init.hash;
}
