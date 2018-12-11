package Word;

use strict;
use warnings;
use List::Util qw/ sum0 /;

our %VALUE = (
    a => 1, b =>  3, c => 3, d => 2, e => 1,
    f => 4, g =>  2, h => 4, i => 1, j => 8,
    k => 5, l =>  1, m => 3, n => 1, o => 1,
    p => 3, q => 10, r => 1, s => 1, t => 1,
    u => 1, v =>  4, w => 4, x => 8, y => 4, z => 10
);

use Class::Tiny qw/ word /;

sub BUILDARGS {
    my ($class, $word) = @_;
    ($word //= "") =~ s/[^[:alpha:]]+//g;
    return {word => lc $word};
}

sub score {
    my ($self, %multipliers) = @_;
    return sum0( map {$VALUE{$_}} split //, $self->word )
         * (2 ** ($multipliers{double} || 0))
         * (3 ** ($multipliers{triple} || 0));
}

1;
