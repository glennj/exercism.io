## no critic (ValuesAndExpressions::ProhibitVersionStrings)
## no critic (Subroutines::ProhibitSubroutinePrototypes)

package Yacht;

use v5.40;
use feature 'switch'; # yeah, yeah, deprecated

use Exporter qw<import>;
our @EXPORT_OK = qw<score>;

use Yacht::Category;
use List::Util qw<sum all>;

sub yacht($dice) {
    my @sorted = sort { $a <=> $b } @$dice;
    return $sorted[0] == $sorted[4] ? 50 : 0;
}

sub choice($dice) {
    return sum @$dice;
}

sub single ( $die, $dice ) {
    return $die * grep { $_ == $die } @$dice;
}

sub full_house($dice) {
    my @sorted = sort { $a <=> $b } @$dice;
    return 0 if $sorted[0] == $sorted[4];    # yacht is not a full house

    my $is_full_house = ( $sorted[0] == $sorted[1] && $sorted[2] == $sorted[4] )
                     || ( $sorted[0] == $sorted[2] && $sorted[3] == $sorted[4] );

    return $is_full_house ? choice $dice : 0;
}

sub four_of_a_kind($dice) {
    my @sorted    = sort { $a <=> $b } @$dice;
    my $is_4_kind = $sorted[0] == $sorted[3] || $sorted[1] == $sorted[4];
    return $is_4_kind ? 4 * $sorted[2] : 0;
}

sub straight( $wanted, $dice ) {
    my @sorted      = sort { $a <=> $b } @$dice;
    my $is_straight = all { $wanted->[$_] == $sorted[$_] } ( 0 .. 4 );
    return $is_straight ? 30 : 0;
}

sub score ( $dice, $category ) {
    my $score = 0;
    for ($category) {
        $score = single 1, $dice             when $Yacht::Category::ONES;
        $score = single 2, $dice             when $Yacht::Category::TWOS;
        $score = single 3, $dice             when $Yacht::Category::THREES;
        $score = single 4, $dice             when $Yacht::Category::FOURS;
        $score = single 5, $dice             when $Yacht::Category::FIVES;
        $score = single 6, $dice             when $Yacht::Category::SIXES;
        $score = full_house $dice            when $Yacht::Category::FULL_HOUSE;
        $score = four_of_a_kind $dice        when $Yacht::Category::FOUR_OF_A_KIND;
        $score = straight [1,2,3,4,5], $dice when $Yacht::Category::LITTLE_STRAIGHT;
        $score = straight [2,3,4,5,6], $dice when $Yacht::Category::BIG_STRAIGHT;
        $score = yacht $dice                 when $Yacht::Category::YACHT;
        $score = choice $dice                when $Yacht::Category::CHOICE;
    }
    return $score;
}

1;
