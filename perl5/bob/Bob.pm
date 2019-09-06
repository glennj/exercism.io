package Bob;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(hey);

sub hey {
    local $_ = shift;   # define the default variable for this scope

    my $q = /[?]\s*$/;                         # question
    my $Y = /[[:alpha:]]/ && $_ eq uc;         # yelling
    my $s = /^\s*$/;                           # silence

    return "Calm down, I know what I'm doing!" if $q and $Y;
    return "Sure."                             if $q;
    return "Whoa, chill out!"                  if $Y;
    return "Fine. Be that way!"                if $s;
    return "Whatever.";
}

1;
