#include "Treap.h"

#include "RandomGenerator.h"

struct Treap::Node
{
    int x, y, cnt;
    Node *left, *right;

    Node(int _x)
        : x(_x)
        , y(get_key())
        , cnt(1)
        , left(nullptr)
        , right(nullptr)
    {
    }

    ~Node()
    {
        delete left;
        delete right;
    }

    void recalc()
    {
        cnt = 1;
        if (left)
            cnt += left->cnt;
        if (right)
            cnt += right->cnt;
    }
};
int Treap::get_key()
{
    return 1e9 * get_random_number();
}
Treap::Treap()
    : root(nullptr)
{
}
Treap::Node * Treap::merge(Node * left, Node * right) const
{
    if (!left || !right)
        return left ? left : right;
    if (left->y < right->y) {
        left->right = merge(left->right, right);
        left->recalc();
        return left;
    }
    else {
        right->left = merge(left, right->left);
        right->recalc();
        return right;
    }
}
void Treap::split(Node * v, int x, Node *& left, Node *& right)
{
    left = right = nullptr;
    if (!v)
        return;
    if (v->x < x) {
        split(v->right, x, v->right, right);
        left = v;
    }
    else {
        split(v->left, x, left, v->left);
        right = v;
    }
    v->recalc();
}
bool Treap::contains(int value) const
{
    Node * v;
    v = root;

    while (v != nullptr) {
        if (value == v->x) {
            return true;
        }
        else if (v->left != nullptr && value < v->x) {
            v = v->left;
        }
        else {
            v = v->right;
        }
    }

    return false;
}
bool Treap::insert(int value)
{
    if (contains(value)) {
        return false;
    }
    Node *left, *right;
    split(root, value, left, right);
    root = merge(merge(left, new Node(value)), right);
    return true;
}
bool Treap::remove(int value)
{
    if (!contains(value)) {
        return false;
    }
    Node *left, *m, *right;
    split(root, value, left, m);
    split(m, value + 1, m, right);
    delete m;
    root = merge(left, right);
    return true;
}
std::size_t Treap::size() const
{
    return root ? root->cnt : 0;
}
bool Treap::empty() const
{
    return root == nullptr;
}
void Treap::inorder(std::vector<int> & a, Node * v) const
{
    if (v != nullptr) {
        inorder(a, v->left);
        a.push_back(v->x);
        inorder(a, v->right);
    }
}
std::vector<int> Treap::values() const
{
    std::vector<int> a;
    a.reserve(Treap::size());
    inorder(a, root);
    return a;
}
Treap::~Treap()
{
    delete root;
}
