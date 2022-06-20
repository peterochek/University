#pragma once

#include "body.h"

#include <memory>

class BHTreeNode
{
public:
    BHTreeNode(const Quadrant & q);

    void insert(const Body & b);

    void update_force(Body & b);

    Quadrant get_quadrant(Cartesian p) const;
    std::unique_ptr<BHTreeNode> create_node(const Quadrant & q);
    int get_idx(const Quadrant & q) const;

    void reset(const Quadrant & q);

    void update(const std::vector<Body> & All, double galaxySize);

private:
    std::vector<std::unique_ptr<BHTreeNode>> nodes;

    Body me;
    Quadrant area;
    int bodies = 0;
};

using Track = std::vector<Cartesian>;

class PositionTracker
{
protected:
    PositionTracker(const std::string & filename);

public:
    virtual Track track(const std::string & body_name, size_t end_time, size_t time_step) = 0;

    virtual ~PositionTracker() = default;

protected:
    double galaxySize;
    std::vector<Body> All;
};

class BasicPositionTracker : public PositionTracker
{
public:
    BasicPositionTracker(const std::string & filename);
    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;

    ~BasicPositionTracker() = default;
};

class FastPositionTracker : public PositionTracker
{
public:
    FastPositionTracker(const std::string & filename);
    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;

    ~FastPositionTracker() = default;
};
