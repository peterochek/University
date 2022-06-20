#pragma once

#include "quadrant.h"

#include <string>
#include <valarray>

class Body
{
public:
    Body() = default;

    Body(Cartesian position, Cartesian speed, double m, const std::string & name);

    double distance(const Body & b) const;

    void add_force(const Body & b);

    void reset_force();

    void update(double delta_t);

    friend std::ostream & operator<<(std::ostream & strm, const Body & b);

    bool in(Quadrant q) const;
    void plus(const Body & b);

    void reset();

    Cartesian get_position() const;

    std::string_view get_name() const;

private:
    std::string name = "empty";
    Cartesian position;
    Cartesian velocity;
    Cartesian acceleration;
    Cartesian force;
    double m = 0;
};
