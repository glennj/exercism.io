package ListOps;

## no critic (Subroutines::ProhibitBuiltinHomonyms)

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(length map reduce);

# Without modifying the contents of the array ref (no `shift`)
# Without using `scalar` or `$#list` which are `length` in disguise

sub reduce {
    my ($func, $list) = @_;
    my $accumulator;
    my $idx = 0;
    for my $element (@$list) {
        $accumulator = $idx
            ? $func->($accumulator, $element)
            : $element;
        $idx++;
    }
    return $accumulator;
}

# all the other list ops can be implemented with `reduce`

sub length {
    my ($list) = @_;
    return reduce sub {$_[0] + 1}, [0, @$list];
}

sub map {
    my ($func, $list) = @_;
    return reduce sub {
        my ($acc, $elem) = @_;
        push @$acc, $func->($elem);
        $acc;
    }, [[], @$list];
}

sub grep {
    my ($func, $list) = @_;
    return reduce sub {
        my ($acc, $elem) = @_;
        push @$acc, $elem if $func->($elem); 
        $acc;
    }, [[], @$list];
}

1;
