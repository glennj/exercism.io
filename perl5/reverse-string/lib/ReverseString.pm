package ReverseString;

## no critic (Subroutines::ProhibitSubroutinePrototypes, ValuesAndExpressions::ProhibitVersionStrings)

use v5.40;
use Unicode::GCString;

use Exporter qw<import>;
our @EXPORT_OK = qw<str_reverse>;

sub str_reverse ($text) {
    my $s = Unicode::GCString->new($text);

    # we can write 
    #   return join '', reverse $s->as_array;
    # but let's avoid using `reverse`

    my @reversed;
    while ( my $gc = $s->next ) {
        unshift @reversed, $gc;
    }
    return join '', @reversed;
}

1;
