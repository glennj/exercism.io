# Declare package 'Bob'
package Bob;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(hey);

sub hey {
    local $_ = shift;
    my $q = /[?]\s*$/;
    my $Y = /[[:alpha:]]/ && $_ eq uc;
    my $s = /^\s*$/;

    if ($q and $Y) { return "Calm down, I know what I'm doing!" }
    elsif ($q)     { return "Sure." }
    elsif ($Y)     { return "Whoa, chill out!" }
    elsif ($s)     { return "Fine. Be that way!" }
    else           { return "Whatever." }
}

1;
