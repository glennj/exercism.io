package House;
use strictures 2;
use List::Util qw/ reduce /;
use Class::Tiny;

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

sub verse {
    my $n = pop() - 1;
    return reduce
        { $a . "that $ACTORS[$b]{what} the $ACTORS[$b-1]{who}\n" }
        "This is the $ACTORS[$n]{who}\n",
        reverse 1 .. $n;
}

sub recite {
    my $self = shift;
    return join "\n", map { $self->verse($_) } 1 .. @ACTORS;
}

1;
