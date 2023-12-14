package MinesweeperBoard;

use 5.024;
#use strictures 2;
use strict;
use warnings;
use Carp;
use List::Util  qw/ all /;
use Class::Tiny qw/ board size input /;

# validate and create the matrix
sub BUILDARGS {
    my ($class, $lines) = @_;
    my $size = {
        rows => scalar($lines->@*),
        cols => length($lines->[0])
    };
    return {input => $lines, size => $size, board => []};
}

# perform the bomb-adjacency count
sub BUILD {
    my $self = shift;
    $self->populate_board;

    for (my $r = 0; $r < $self->size->{rows}; $r++) {
        for (my $c = 0; $c < $self->size->{cols}; $c++) {
            if ($self->board->[$r][$c] == 9) {
                $self->incr_neighbours($r, $c);
            }
        }
    }
    return;
}

sub incr_neighbours {
    my ($self, $r, $c) = @_;
    for my $dr (-1 .. 1) {
        for my $dc (-1 .. 1) {
            my ($rr, $cc) = ($r + $dr, $c + $dc);
            next if $rr == -1 or $rr == $self->size->{rows}
                 or $cc == -1 or $cc == $self->size->{cols};
            next if $self->board->[$rr][$cc] == 9;
            $self->board->[$rr][$cc] += 1;
        }
    }
    return;
}

sub populate_board {
    my $self = shift;
    push $self->board->@*,
        map {[split '']}
        map {tr/ */09/r}
        $self->input->@*;
    return;
}

sub annotate {
    my ($self) = @_;
    # the reverse of populate_board:
    return [
        map {tr/09/ */r}
        map {join '', $_->@*}
        $self->board->@*
    ];
}

1;
