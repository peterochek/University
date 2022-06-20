#include "nbody.h"

#include "constants.h"

#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>

PositionTracker::PositionTracker(const std::string & filename)
{
    std::fstream File;
    File.open(filename);
    if (!File) {
        std::cerr << "No file with such name!" << std::endl;
    }
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(File, line)) {
        lines.emplace_back(line);
    }

    std::stringstream get_size(lines[0]);
    get_size >> galaxySize;

    for (std::size_t i = 1; i < lines.size(); ++i) {
        std::stringstream ss(lines[i]);

        double x0;
        double y0;
        double vx0;
        double vy0;
        double m;
        std::string name;

        ss >> x0 >> y0 >> vx0 >> vy0 >> m >> name;

        All.emplace_back(Cartesian(x0, y0), Cartesian(vx0, vy0), m, name);
    }
}

BasicPositionTracker::BasicPositionTracker(const std::string & filename)
    : PositionTracker(filename)
{
}
Track BasicPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track t;
    for (size_t i = 0; i < end_time; i += time_step) {
        for (Body & outer : All) {
            outer.reset_force();
            for (const Body & inner : All) {
                outer.add_force(inner);
            }
        }
        for (Body & outer : All) {
            outer.update(time_step);
        }
    }
    for (const Body & b : All) {
        if (b.get_name() == body_name) {
            t.emplace_back(b.get_position());
            break;
        }
    }
    return t;
}

FastPositionTracker::FastPositionTracker(const std::string & filename)
    : PositionTracker(filename)
{
}
Track FastPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track t;
    BHTreeNode tree(Quadrant(Cartesian(0, 0), 0));
    tree.reset(Quadrant(Cartesian(0, 0), galaxySize));
    for (const Body & outer : All) {
        tree.insert(outer);
    }

    for (size_t i = 0; i < end_time; i += time_step) {
        for (Body & outer : All) {
            outer.reset_force();
            tree.update_force(outer);
        }
        for (Body & outer : All) {
            outer.update(time_step);
        }
        tree.update(All, galaxySize);
    }
    for (const Body & b : All) {
        if (b.get_name() == body_name) {
            t.emplace_back(b.get_position());
            break;
        }
    }
    return t;
}

Quadrant BHTreeNode::get_quadrant(const Cartesian p) const
{
    std::optional<Quadrant> returnable;
    auto checker = [&p, &returnable](Quadrant check) {
        if (check.contains(p)) {
            returnable = check;
        }
    };
    checker(area.ne());
    checker(area.nw());
    checker(area.sw());
    checker(area.se());
    if (returnable) {
        return *returnable;
    }
    else {
        throw std::runtime_error("Cant get quadrant");
    }
}
std::unique_ptr<BHTreeNode> BHTreeNode::create_node(const Quadrant & q)
{
    std::unique_ptr<BHTreeNode> returnable;
    auto checker = [&q, &returnable](Quadrant check) {
        if (check.get_center() == q.get_center()) {
            returnable = std::make_unique<BHTreeNode>(check);
        }
    };
    checker(area.nw());
    checker(area.ne());
    checker(area.sw());
    checker(area.se());

    if (returnable) {
        return returnable;
    }
    else {
        throw std::runtime_error("Cant create node");
    }
}
BHTreeNode::BHTreeNode(const Quadrant & q)
    : nodes(4)
    , area(q)
{
}

void BHTreeNode::insert(const Body & b)
{
    if (bodies > 1) {
        Quadrant q = get_quadrant(b.get_position());
        int idx = get_idx(q);
        if (!nodes[idx]) {
            nodes[idx] = create_node(q);
        }

        me.plus(b);

        nodes[idx]->insert(b);
    }
    else if (bodies == 1) {
        Quadrant q = get_quadrant(me.get_position());
        int idx = get_idx(q);
        if (!nodes[idx]) {
            nodes[idx] = create_node(q);
        }
        nodes[idx]->insert(me);
        me.plus(b);

        q = get_quadrant(b.get_position());
        idx = get_idx(q);
        if (!nodes[idx]) {
            nodes[idx] = create_node(q);
        }
        nodes[idx]->insert(b);
    }
    else if (bodies == 0) {
        me = b;
    }
    bodies++;
}
void BHTreeNode::reset(const Quadrant & q)
{
    for (int i = 0; i < 4; ++i) {
        nodes[i] = nullptr;
    }

    area = q;
    bodies = 0;

    me.reset();
}
void BHTreeNode::update_force(Body & b)
{
    if (bodies == 1) {
        b.add_force(me);
    }
    else {
        double r = b.distance(me);
        double d = area.length();
        if (d / r <= Theta) {
            b.add_force(me);
        }
        else {
            for (int q = 0; q < 4; ++q) {
                if (nodes[q]) {
                    nodes[q]->update_force(b);
                }
            }
        }
    }
}
int BHTreeNode::get_idx(const Quadrant & q) const
{
    if (q.get_center() == area.ne().get_center()) {
        return 0;
    }
    if (q.get_center() == area.nw().get_center()) {
        return 1;
    }
    if (q.get_center() == area.sw().get_center()) {
        return 2;
    }
    if (q.get_center() == area.se().get_center()) {
        return 3;
    }
    else {
        throw std::runtime_error("Cant determine quadrant");
    }
}
void BHTreeNode::update(const std::vector<Body> & All, double galaxySize)
{
    this->reset(Quadrant(Cartesian(0, 0), galaxySize));
    for (const Body & outer : All) {
        this->insert(outer);
    }
}
