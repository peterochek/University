#pragma once

#include <string>
#include <vector>

class Board
{
public:
    Board() = default;

    explicit Board(const std::vector<std::vector<unsigned>> & data);

    static Board create_random(unsigned size);

    std::size_t size() const;

    unsigned hamming() const;

    unsigned manhattan() const;

    bool is_goal() const;

    bool is_solvable() const;

    std::string to_string() const;

    std::size_t heuristics() const;

    std::size_t from_start() const;

    void update_from_start(const Board & better);

    std::size_t to_end() const;

    std::size_t possible() const;

    static std::vector<Board> nearby(const Board & current);

    std::size_t get_hash() const;

    friend bool operator<(const Board & lhs, const Board & rhs);

    friend bool operator==(const Board & lhs, const Board & rhs);

    friend bool operator!=(const Board & lhs, const Board & rhs);

    friend std::ostream & operator<<(std::ostream & out, const Board & board)
    {
        return out << board.to_string();
    }

    const std::vector<unsigned> & operator[](std::size_t row) const
    {
        return data[row];
    };

    ~Board() = default;

private:
    std::vector<std::vector<unsigned>> data;

    std::size_t moves = 0;

    std::size_t inversions() const;

    static std::size_t hasher(Board & init);

    std::pair<std::size_t, std::size_t> gap() const;

    std::size_t hash;
};

class HashBoard
{
public:
    std::size_t operator()(const Board & initial) const
    {
        return initial.get_hash();
    }
};
