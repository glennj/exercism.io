package Queen;

use v5.38;
use Moo;

our $BOARD_SIZE = 8;

has row => (
    is => 'ro',
    isa => sub {
        my $row = shift;
        die 'row not on board' unless 0 <= $row && $row < $BOARD_SIZE
    },
);

has column => (
    is => 'ro',
    isa => sub {
        my $col = shift;
        die 'column not on board' unless 0 <= $col && $col < $BOARD_SIZE
    },
);

sub can_attack ( $self, $other ) {
    my $d_row = abs($self->row - $other->row);
    my $d_col = abs($self->column - $other->column);
    return ($d_row == 0 or $d_col == 0 or $d_row == $d_col);
}
