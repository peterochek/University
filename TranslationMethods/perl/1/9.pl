while (<>) {
    print if /^(?:\S.*\S|\S{0,1})$/;
}