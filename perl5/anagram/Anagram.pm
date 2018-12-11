package Anagram;
use strictures 2;

sub match {
    my ($word, @words) = @_;
    my $toKey = sub { join '', sort split '', lc shift };
    my $key = $toKey->($word);
    return [grep {$word ne $_ and $key eq $toKey->($_)} @words];
}

1;
