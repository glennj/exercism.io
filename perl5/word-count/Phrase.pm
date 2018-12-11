package Phrase;
use strictures 2;

sub word_count {
    my %count;
    $count{$_}++ for grep {$_} map {s/\W//g; lc} split ' ', shift;
    return \%count;
}

1;
