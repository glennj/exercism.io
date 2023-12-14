package Raindrops;

#use strictures 2;
use strict;
use warnings;
#use Exporter::Easiest 'OK => raindrop';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ raindrop /;

# this module implements an "ordered hash" -- insertion order is preserved.
use Tie::IxHash;

tie our %SOUND, 'Tie::IxHash';
%SOUND = ( 3 => 'Pling', 5 => 'Plang', 7 => 'Plong' );

sub raindrop {
    my ($number) = @_;
    my $pling = join '',
                map  { $_->[0] }
                grep { $_->[1] == 0 }
                map  { [$SOUND{$_}, $number % $_] }
                keys %SOUND;
    return $pling || $number;
}

1;
