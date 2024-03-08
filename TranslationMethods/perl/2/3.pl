while (<>) {
    s/\b(?i)[aA]+\b/argh/;
    print;
}