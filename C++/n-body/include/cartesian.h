#pragma once

#include "constants.h"

struct Cartesian
{
    Cartesian(double x = 0, double y = 0)
        : x(x)
        , y(y)
    {
    }
    bool operator==(const Cartesian & other) const
    {
        return std::hypot(x - other.x, y - other.y) < eps;
    }

    double x;
    double y;
};