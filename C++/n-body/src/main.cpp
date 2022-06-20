#include "nbody.h"

#include <bits/stdc++.h>

int main()
{
    std::string filename = "/home/peter/CLionProjects/n-body-peterochek/test/etc/planets.txt";

    size_t end_time = 1000;
    size_t time_step = 1;

    BasicPositionTracker basic_tracker = BasicPositionTracker(filename);
    FastPositionTracker fast_tracker = FastPositionTracker(filename);

    Track basic_track = basic_tracker.track("Venus", end_time, time_step);
    Track fast_track = fast_tracker.track("Venus", end_time, time_step);

    std::cout << "Basic track: " << std::setprecision(12) << basic_track.back().x << " " << basic_track.back().y << std::endl;
    std::cout << "Fast track:  " << std::setprecision(12) << fast_track.back().x << " " << fast_track.back().y << std::endl;
}
