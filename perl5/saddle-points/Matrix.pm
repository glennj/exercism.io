## no critic (RegularExpressions::RequireExtendedFormatting)

package Matrix;

use 5.024;
use strictures 2;
use List::Util  qw/ reduce min max any /;
use List::MoreUtils qw/ firstidx /;

sub new {
    my ($class, $text) = @_;
    my @rows = map { [split] } split /\n/, $text;

    # assume the matrix is square
    my $cols = reduce {$a->[$b] = [map {$_->[$b]} @rows]; $a} [], 0 .. $rows[0]->$#*;

    return bless {rows => \@rows, cols => $cols}, $class;
}

sub rows    { return shift->{rows}->[pop] }
sub columns { return shift->{cols}->[pop] }

sub saddle_points {
    my $self = shift;
    my (@rows_max, @cols_min);
    for (my $i = 0; $i < $self->{rows}->@*; $i++) {
        $rows_max[$i] = _max_idx($self->{rows}->[$i]->@*);
    }
    for (my $j = 0; $j < $self->{cols}->@*; $j++) {
        $cols_min[$j] = _min_idx($self->{cols}->[$j]->@*);
    }
    my @saddle_points;
    for (my $r = 0; $r < $self->{rows}->@*; $r++) {
        for (my $c = 0; $c < $self->{cols}->@*; $c++) {
            if (_in($c, $rows_max[$r]->@*) and _in($r, $cols_min[$c]->@*)) {
                push @saddle_points, [$r, $c];
            }
        }
    }
    return \@saddle_points;
}

sub _max_idx {
    my (@list) = @_;
    my $max = max @list;
    return [grep {$max == $list[$_]} 0..$#list];
}

sub _min_idx {
    my (@list) = @_;
    my $min = min @list;
    return [grep {$min == $list[$_]} 0..$#list];
}

sub _in {
    my ($elem, @list) = @_;
    return any {$elem == $_} @list;
}

1;

