package BeerSong;

use 5.024;
use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw(verse sing);

our $MAX    = 99;
our $LIQUID = 'beer';
our $WHERE  = "on the wall";

sub verse {
    my $n = shift;
    return join "\n", firstLine($n), secondLine($n), '';
}

sub firstLine {
    my $n = shift;
    my $b = bottles($n);
    my $B = ucfirst($b);
    return "$B $WHERE, $b.";
}

sub secondLine {
    my $n = shift;
    my $b = bottles($n ? $n - 1 : $MAX);
    my $it = $n == 1 ? "it" : "one";
    my $task = $n ? "Take $it down and pass it around" : "Go to the store and buy some more";
    return "$task, $b $WHERE.";
}

sub bottles {
    my $n = shift;
    my $s = $n == 1 ? "" : "s";
    $n ||= "no more";
    return "$n bottle$s of $LIQUID";
}

sub sing {
    # input is a hash ref with 2 entries:
    my ($bottles, $verses) = (shift)->@{qw/bottles verses/};
    my $from = $bottles - $verses + 1;
    my $to = $bottles;
    return join '', map {verse($_) . "\n"} reverse $from .. $to;
}

1;
