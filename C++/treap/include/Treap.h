#pragma once

#include <vector>

class Treap
{
    struct Node;

    Node * merge(Node * left, Node * right) const;
    void split(Node * v, int x, Node *& left, Node *& right);
    void inorder(std::vector<int> & a, Node * v) const;

    static int get_key();

    Node * root;

public:
    Treap();

    bool contains(int value) const;
    bool insert(int value);
    bool remove(int value);

    std::size_t size() const;
    bool empty() const;

    std::vector<int> values() const;

    ~Treap();
};
