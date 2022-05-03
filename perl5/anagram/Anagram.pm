package Anagram;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easy (OK => ['match_anagrams']);
use Exporter     qw/ import /;
our @EXPORT_OK = qw/ match_anagrams /;

sub match_anagrams {
    my ($data) = @_;
    my $subj = lc $data->{subject};
    my @Words = @{$data->{candidates}};

    my $toKey = sub { join '', sort split '', shift };
    my $key = $toKey->($subj);

    return [
        map {$_->[1]}
        grep {$subj ne $_->[0] and $key eq $toKey->($_->[0])} 
        map {[lc, $_]}
        @Words
    ];
}

1;
