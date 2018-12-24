package Triplet;
use strictures 2;
use Carp;
use List::Util;
use Class::Tiny qw/ sides conditions /;

sub BUILDARGS {
    my ($class, @args) = @_;
    if (@args == 3) {
        my ($a, $b, $c) = sort {$a <=> $b} @args;
        croak 'ArgumentError' if $a + $b <= $c;
        return { sides => [$a, $b, $c] } ;
    }
    if (@args == 1) {
        # In the 1 arg case, we're passing a hash of
        # conditions for the finding of pythagorean triplets
        return { conditions => $args[0] };
    }
    croak "ArgumentError: @args";
}

sub sum     { return List::Util::sum     shift->sides->@* }
sub product { return List::Util::product shift->sides->@* }

sub is_pythagorean {
    my ($a, $b, $c) = shift->sides->@*;
    return $a * $a + $b * $b == $c * $c;
}

sub products {
    my $self = shift;
    my $triplets = find_triplets( $self->conditions );
    return [ map { $_->product } @$triplets ];
}

sub find_triplets {
    my $conditions = shift;
    croak 'ArgumentError'
        unless ref $conditions eq 'HASH'
           and exists $conditions->{max_factor};

    my ($min, $max, $sum) = $conditions->@{qw/ min_factor max_factor sum /};
    $min //= 1;

    my @pythagorean_triplets;
    for (my $c = $max; $c > $min; $c--) {
        for (my $b = $c - 1; $b >= $min; $b--) {
            my $a = sqrt( $c*$c - $b*$b );
            next if $a != int($a);
            next if $a <= $b;

            my $triplet = __PACKAGE__->new($a, $b, $c);
            next unless $triplet->is_pythagorean;
            next if (defined $sum and $sum != $triplet->sum);

            push @pythagorean_triplets, $triplet;
        }
    }
    return \@pythagorean_triplets;
}

1;
