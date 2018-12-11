package Squares;
use strictures 2;
use List::Util  qw/ sum0 /;

sub new {
    my ($class, $n) = @_;
    return bless \$n, $class;
}

sub square_of_sum {
    my $self = shift;
    return ( sum0 1 .. $$self ) ** 2;
}

sub sum_of_squares {
    my $self = shift;
    return sum0 map {$_ ** 2} 1 .. $$self;
}

sub difference {
    my $self = shift;
    return $self->square_of_sum - $self->sum_of_squares;
}

1;
