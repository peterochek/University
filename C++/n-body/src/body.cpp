#include "body.h"

#include "constants.h"

Body::Body(Cartesian position, Cartesian velocity, double m, const std::string & name)
    : name(name)
    , position(position)
    , velocity(velocity)
    , m(m)
{
}
double Body::distance(const Body & b) const
{
    return std::hypot(position.x - b.position.x, position.y - b.position.y) + eps;
}
void Body::reset_force()
{
    force.x = 0;
    force.y = 0;
}
void Body::add_force(const Body & b)
{
    double f = G * (b.m / distance(b)) * (m / distance(b));

    force.x += f * (b.position.x - position.x) / distance(b);
    force.y += f * (b.position.y - position.y) / distance(b);
}
void Body::update(double delta_t)
{
    acceleration.x = force.x / m;
    acceleration.y = force.y / m;

    velocity.x += acceleration.x * delta_t;
    velocity.y += acceleration.y * delta_t;

    position.x += velocity.x * delta_t;
    position.y += velocity.y * delta_t;
}
bool Body::in(const Quadrant q) const
{
    bool horizontal = (q.get_center().x - q.length() / 2 <= position.x && position.x <= q.get_center().x + q.length() / 2);
    bool vertical = (q.get_center().y - q.length() / 2 <= position.y && position.y <= q.get_center().y + q.length() / 2);

    return horizontal && vertical;
}
void Body::plus(const Body & b)
{
    position.x = (position.x * m + b.position.x * b.m) / (m + b.m);
    position.y = (position.y * m + b.position.y * b.m) / (m + b.m);
    m += b.m;
}
void Body::reset()
{
    position = {0, 0};
    velocity = {0, 0};
    acceleration = {0, 0};
    force = {0, 0};
    m = 0;
    name = "empty";
}
Cartesian Body::get_position() const
{
    return position;
}
std::string_view Body::get_name() const
{
    return name;
}
std::ostream & operator<<(std::ostream & strm, const Body & b)
{
    std::string tmp = std::to_string(b.position.x) + " " + std::to_string(b.position.y) + " " + b.name;
    return strm << tmp;
}