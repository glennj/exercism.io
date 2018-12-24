package Raindrops;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(raindrop);
use Tie::IxHash;

tie our %SOUND, 'Tie::IxHash';
%SOUND = ( 3 => 'Pling', 5 => 'Plang', 7 => 'Plong' );

sub raindrop {
    my ($number) = @_;
    my $pling =
        join '',
        map  { $_->[0] }
        grep { $_->[1] == 0 }
        map  { [$SOUND{$_}, $number % $_] }
        keys %SOUND;
    return $pling || $number;
}

1;
