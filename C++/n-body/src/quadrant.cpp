#include "quadrant.h"

Quadrant::Quadrant(Cartesian center, double length)
    : center(center)
    , size(length)
{
}
double Quadrant::length() const
{
    return size;
}
Cartesian Quadrant::get_center() const
{
    return center;
}
bool Quadrant::contains(Cartesian p) const
{
    bool horizontal = (center.x - size / 2 <= p.x && p.x <= center.x + size / 2);
    bool vertical = (center.y - size / 2 <= p.y && p.y <= center.y + size / 2);

    return horizontal && vertical;
}
Quadrant Quadrant::ne() const
{
    return Quadrant(Cartesian(center.x + size / 4, center.y + size / 4), size / 2);
}
Quadrant Quadrant::nw() const
{
    return Quadrant(Cartesian(center.x - size / 4, center.y + size / 4), size / 2);
}
Quadrant Quadrant::se() const
{
    return Quadrant(Cartesian(center.x + size / 4, center.y - size / 4), size / 2);
}
Quadrant Quadrant::sw() const
{
    return Quadrant(Cartesian(center.x - size / 4, center.y - size / 4), size / 2);
}
std::ostream & operator<<(std::ostream & strm, const Quadrant & q)
{
    std::string tmp = std::to_string(q.center.x) + " " + std::to_string(q.center.y) + " " + std::to_string(q.size);
    return strm << tmp;
}
