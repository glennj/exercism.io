package Knapsack;

## no critic (ValuesAndExpressions::ProhibitVersionStrings)

use v5.42;
use List::Util qw(max);

use Exporter qw<import>;
our @EXPORT_OK = qw<maximum_value>;

# https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem

sub maximum_value ( $items, $weight ) {
    my $count = $items->@*;

    # 2-d array
    my @m = map { [ (0) x ( $weight + 1 ) ] } ( 1 .. $count + 1 );

    for ( my $i = 1 ; $i <= $count ; $i++ ) {
        foreach my $w ( 1 .. $weight ) {
            my ( $iw, $iv ) = @{ $items->[ $i - 1 ] }{qw( weight value )};
            if ( $iw > $w ) {
                $m[$i][$w] = $m[ $i - 1 ][$w];
            }
            else {
                $m[$i][$w] = max $m[ $i - 1 ][$w],
                  $iv + $m[ $i - 1 ][ $w - $iw ];
            }
        }
    }
    return $m[$count][$weight];
}

1;
