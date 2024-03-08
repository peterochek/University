my $word_pattern = qr/\b\w\w*\b/;

while (<>) {
    s/($word_pattern)([^\w]+)($word_pattern)/$3$2$1/;
    print;
}
