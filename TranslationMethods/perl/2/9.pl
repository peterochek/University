while (<>) {
    s/\([^)]*?\)/()/g;
    print;
}