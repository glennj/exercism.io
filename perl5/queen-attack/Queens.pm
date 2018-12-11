package Queens;

use strictures 2;
use Carp;
use Class::Tiny qw( white black );

# validation
sub BUILD {
    my ($self, $params) = @_;

    # default positions
    $self->white( [0, 3] ) unless defined $self->white;
    $self->black( [7, 3] ) unless defined $self->black;

    croak 'ArgumentError: invalid position'
        unless valid_position( $self->white )
           and valid_position( $self->black );

    croak 'ArgumentError: same position' 
        if $self->white->[0] == $self->black->[0]
       and $self->white->[1] == $self->black->[1];

    return;
}

sub valid_position {
    my $pos = shift;
    return (
        ref $pos eq 'ARRAY' 
        and @$pos == 2
        and 0 <= $pos->[0] and $pos->[0] <= 7
        and 0 <= $pos->[1] and $pos->[1] <= 7 
    );
}

sub can_attack {
    my $self = shift;
    my $d_row = abs( $self->white->[0] - $self->black->[0] );
    my $d_col = abs( $self->white->[1] - $self->black->[1] );
    return ($d_row == 0 or $d_col == 0 or $d_row == $d_col);
}

sub to_string {
    my $self = shift;
    my @board = map { [ ('O') x 8 ] } 1 .. 8;
    $board[ $self->white->[0] ][ $self->white->[1] ] = 'W';
    $board[ $self->black->[0] ][ $self->black->[1] ] = 'B';
    return join '', map { "@$_\n" } @board;
}

1;
