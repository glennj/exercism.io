package Beer;
use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw(verse sing);

our $MAX    = 99;
our $LIQUID = lc __PACKAGE__;
our $WHERE  = "on the wall";

sub verse {
    my $n = shift;
    return join "\n", first($n), second($n), '';
}

sub first {
    my $n = shift;
    my $b = bottles($n);
    my $B = ucfirst($b);
    return "$B $WHERE, $b.";
}

sub second {
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
    my ($from, $to) = @_;
    $from //= $MAX;
    $to //= 0;
    return join '', map {verse($_) . "\n"} reverse $to .. $from;
}

1;
