package Bob;
use strictures 2;
use Exporter::Easiest 'OK => hey';

sub hey {
    local $_ = shift;   # define the default variable for this scope

    my $q = /[?]\s*$/;                         # question
    my $Y = /[[:upper:]]/ && ! /[[:lower:]]/;  # yelling
    my $s = /^\s*$/;                           # silence

    return "Calm down, I know what I'm doing!" if $q and $Y;
    return "Sure."                             if $q;
    return "Whoa, chill out!"                  if $Y;
    return "Fine. Be that way!"                if $s;
    return "Whatever.";
}

1;
