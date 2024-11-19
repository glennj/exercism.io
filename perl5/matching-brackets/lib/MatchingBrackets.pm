package MatchingBrackets;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<has_matching_brackets>;

sub has_matching_brackets ($text) {

    # remove non-brackets
    $text =~ tr/()[]{}//cd;

    # repeatedly remove contiguous pairs until no more found
    while ( $text =~ s/ \(\) | \[\] | \{\} //xg ) { }

    # a non-empty string has non-matching brackets
    return $text eq '';
}

1;
