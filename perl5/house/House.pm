package House;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => recite';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ recite /;

use List::Util qw/ reduce /;

our @ACTORS = (
    { who => 'house that Jack built.' },
    { who => 'malt', what => 'lay in' },
    { who => 'rat', what => 'ate' },
    { who => 'cat', what => 'killed' },
    { who => 'dog', what => 'worried' },
    { who => 'cow with the crumpled horn', what => 'tossed' },
    { who => 'maiden all forlorn', what => 'milked' },
    { who => 'man all tattered and torn', what => 'kissed' },
    { who => 'priest all shaven and shorn', what => 'married' },
    { who => 'rooster that crowed in the morn', what => 'woke' },
    { who => 'farmer sowing his corn', what => 'kept' },
    { who => 'horse and the hound and the horn', what => 'belonged to' },
);

sub recite {
    my ($from, $to) = (shift)->@{'startVerse', 'endVerse'};
    return [map { verse($_) } $from..$to];
}

sub verse {
    my ($n) = @_;
    $n -= 1;
    return reduce
        { $a . " that $ACTORS[$b]{what} the $ACTORS[$b-1]{who}" }
            "This is the $ACTORS[$n]{who}",
            reverse 1 .. $n;
}

1;
