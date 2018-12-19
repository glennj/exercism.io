package Minesweeper;
use strictures 2;
use Carp;
use List::Util  qw/ all /;
use Class::Tiny qw/ board size /;

# validate and create the matrix
sub BUILDARGS {
    my ($class, $board_str) = @_;
    my @lines = split /\n/, $board_str;

    croak 'ArgumentError'
        unless all {
            length == length($lines[0])
            and /^ (?: [+] -+ [+] ) | (?: [|] [ *]+ [|] ) $/x
        } @lines;

    my $board = [
        map {[ split '' ]}
        map { s/ /0/gr }
        map { s/^[|] | [|]$//xgr }
        @lines[1 .. $#lines - 1]
    ];
    my $size = {rows => 1 + $board->$#*, cols => 1 + $board->[0]->$#* };

    return { board => $board, size => $size };
}

# perform the bomb-adjacency count
sub BUILD {
    my $self = shift;
    for (my $r = 0; $r < $self->size->{rows}; $r++) {
        for (my $c = 0; $c < $self->size->{cols}; $c++) {
            if ($self->board->[$r][$c] eq "*") {
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
            next if $self->board->[$rr][$cc] eq '*';
            $self->board->[$rr][$cc] += 1;
        }
    }
    return;
}

sub to_string {
    my ($self, $show_count) = @_;
    my $digits = ($show_count || 0) ? '0' : '0-9';
    my $header = '+' . '-' x $self->size->{cols} . '+';
    return
        join "\n", $header, (
            map { "|$_|" }
            map { s/[$digits]/ /gr }
            map { join '', @$_ }
            $self->board->@*
        ), $header, '';
}
sub show_count { return shift->to_string(1) }

sub count_adjacent_bombs {
    return __PACKAGE__->new( shift )->show_count;
}

1;
