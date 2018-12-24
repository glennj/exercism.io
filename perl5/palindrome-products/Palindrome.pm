package Palindrome;
use strict;
use warnings;
use feature 'postderef';

sub new {
    my ($class, $attrs) = @_;
    $attrs //= {};
    my $min = $attrs->{min_factor} // 1;
    my $max = $attrs->{max_factor} // 10;

    my $factors = {};
    for (my $i = $min; $i <= $max; $i++) {
        for (my $j = $i; $j <= $max; $j++) {
            if (_is_palindrome($i * $j)) {
                push $factors->{$i * $j}->@*, [$i, $j];
            }
        }
    }

    my $self = {
        factors => $factors,
        products => [sort {$a <=> $b} keys $factors->%*],
    };
    return bless $self, $class;
}

sub _is_palindrome {
    my $input = shift;
    return "".$input eq reverse("".$input);
}

sub _prod_fact {
    my ($self, $idx) = @_;
    my $prod = $self->{products}[$idx];
    return {
        value => $prod,
        factors => $self->{factors}{$prod},
    };
}

sub largest  { return shift->_prod_fact(-1); }
sub smallest { return shift->_prod_fact(0); }

1;
