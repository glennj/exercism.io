package Proverb;

#use strictures 2;
use strict;
use warnings;
#use Exporter::Easiest 'OK => proverb';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ proverb /;

sub proverb {
    my ($terms, $qualifier) = @_;
    $qualifier //= "";
    $qualifier .= " " if $qualifier;
    my @lines = map {
        "For want of a $terms->[$_] the $terms->[$_ + 1] was lost."
    } 0 .. $terms->$#* - 1;
    push @lines, "And all for the want of a $qualifier$terms->[0].";
    return join "\n", @lines;
}

1;

