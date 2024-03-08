my $output = "";
my $leading_trailing_spaces = qr/^\s+|\s+$/;
my $multiple_spaces = qr/ {2,}/;

while (my $curLine = <>) {
    $curLine =~ s/$leading_trailing_spaces//g;
    $curLine =~ s/$multiple_spaces/ /g;

    $output .= "\n$curLine";
}

my $leading_trailing_newlines = qr/^\n+|\n+$/;
my $multiple_newlines = qr/\n(\n)+/;

$output =~ s/$leading_trailing_newlines//g;
$output =~ s/$multiple_newlines/\n\n/g;

print $output;