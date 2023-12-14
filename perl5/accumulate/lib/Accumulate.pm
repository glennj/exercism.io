package Accumulate;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => accumulate';
use Exporter 'import';
our @EXPORT_OK = qw/ accumulate /;

sub accumulate {
    my ($input, $func) = @_;
    $input //= [];
    $func  //= sub { };
    my @result;
    push @result, $func->($_) foreach @$input;
    return \@result;
}

1;
