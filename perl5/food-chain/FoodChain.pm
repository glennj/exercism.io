package FoodChain;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => recite';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ recite /;

our @CHAIN = ( {},
    { name => 'fly' },
    { name => 'spider', tag => 'It wriggled and jiggled and tickled inside her.',
                        extra => ' that wriggled and jiggled and tickled inside her' },
    { name => 'bird',   tag => 'How absurd to swallow a bird!' },
    { name => 'cat',    tag => 'Imagine that, to swallow a cat!' },
    { name => 'dog',    tag => 'What a hog, to swallow a dog!' },
    { name => 'goat',   tag => 'Just opened her throat and swallowed a goat!' },
    { name => 'cow',    tag => "I don't know how she swallowed a cow!" },
    { name => 'horse',  tag => "She's dead, of course!" },
);

sub recite {
    my ($from, $to) = (shift)->@{'startVerse', 'endVerse'};
    my @lines;
    for my $i ($from .. $to) {
        push @lines, "" if $i > $from;
        push @lines, verse($i);
    }
    return \@lines;
}

sub verse {
    my $n = shift;
    my @verse = ();
    push @verse, iknow($CHAIN[$n]);
    if ($n < $#CHAIN) {
        for (my $i = $n; $i >= 2; $i--) {
            push @verse, hunt($CHAIN[$i], $CHAIN[$i - 1]);
        }
        push @verse, ending();
    }
    return @verse;
}

sub iknow {
    my $animal = shift;
    my @know;
    push @know, "I know an old lady who swallowed a $animal->{name}.";
    push @know, $animal->{tag} if exists $animal->{tag};
    return @know;
}

sub hunt {
    my ($predator, $prey) = @_;
    return sprintf "She swallowed the %s to catch the %s%s.",
        $predator->{name},
        $prey->{name},
        $prey->{extra} // '';
}

sub ending { 
    return "I don't know why she swallowed the fly. Perhaps she'll die."
}

1;
