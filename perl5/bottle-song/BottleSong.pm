package BottleSong;

use strict;
use warnings;
use feature qw<say>;

use Exporter qw<import>;
our @EXPORT_OK = qw<sing>;

my %numbers = (
    1  => 'one',
    2  => 'two',
    3  => 'three',
    4  => 'four',
    5  => 'five',
    6  => 'six',
    7  => 'seven',
    8  => 'eight',
    9  => 'nine',
    10 => 'ten',
);

sub sing {
    my ( $bottles, $verses ) = @_;
    my @lines;
    for (my $i = 0; $i < $verses;) {
        push @lines, verse($bottles - $i);
        push @lines, "" if ++$i < $verses;
    }
    return join "\n", @lines;
}

sub bottles {
    return "bottle" . (shift == 1 ? "" : "s");
}

sub verse {
    my $n = shift;
    my @lines;
    push @lines, sprintf("%s green %s hanging on the wall,",
                         ucfirst $numbers{$n},
                         bottles($n));
    push @lines, $lines[0];
    push @lines, "And if one green bottle should accidentally fall,";
    push @lines, sprintf("There'll be %s green %s hanging on the wall.",
                         $numbers{$n - 1} // "no",
                         bottles($n - 1));
    return @lines;
}

1;
