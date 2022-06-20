#pragma once

#include "cartesian.h"

#include <ostream>

class Quadrant
{
public:
    Quadrant(Cartesian center, double length);

    bool contains(Cartesian p) const;
    double length() const;

    Quadrant nw() const;
    Quadrant ne() const;
    Quadrant sw() const;
    Quadrant se() const;

    Cartesian get_center() const;

    friend std::ostream & operator<<(std::ostream & strm, const Quadrant & q);

private:
    Cartesian center;
    double size;
};