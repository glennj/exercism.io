## no critic (RegularExpressions::RequireExtendedFormatting)

package WordCount;


#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => count_words';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ count_words /;

use List::Util 'reduce';

sub count_words {
    my ($sentence) = @_;
    my @words = ($sentence =~ m/[\w']+/g);
    return reduce {
        $b =~ s/^'|'$//g;   # trim leading/trailing quote
        $a->{lc $b}++;
        $a;
    } {}, @words
}

1;
