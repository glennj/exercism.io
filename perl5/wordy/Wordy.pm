package Wordy;

## no critic (RegularExpressions::RequireExtendedFormatting)
use strict;
use warnings;
use Carp;

sub answer {
    local $_ = shift;
    s/[[:punct:]]+\s*$//g;          # remove trailing punctuation

    # replace the operators
    s{\bmultiplied\s+by\b} {*}g;
    s{\bdivided\s+by\b}    {/}g;
    s{\bplus\b}            {+}g;
    s{\bminus\b}           {-}g;

    s/\b[[:alpha:]]+\b//g;          # remove remaining words
    $_ = join ' ', split ' ';       # normalize whitespace

    my $expr = qr/-?\d+ \D -?\d+/;

    # We need at least one arithmetic expression.
    croak 'ArgumentError' unless /$expr/;

    # This method enforces the strict left to right evaluation.
    # We need 2 rounds of evaluation to replace the expression with its result.
    do 1 while s/($expr)/$1/ee;

    return $_;
}

1;
