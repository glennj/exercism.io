package Series;
use strictures 2;
use List::Util qw/ max product /;
use Carp;

sub new {
    my ($class, $input) = @_;
    croak 'ArgumentError' if $input =~ /\D/;
    return bless [ split '', $input ], $class;
}

sub largest_product {
    my ($self, $size) = @_;
    my $last_idx = $self->@* - $size;
    croak 'ArgumentError' if $last_idx < 0;
    return max map { product $self->@[$_ .. $_ + $size - 1] } 0 .. $last_idx;
}

1;
