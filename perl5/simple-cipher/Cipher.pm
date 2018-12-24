package Cipher;

## no critic (RequireExtendedFormatting, RequireArgUnpacking)

use strict;
use warnings;
use 5.020;
use Carp;
use List::MoreUtils qw/ firstidx pairwise /;

our @ALPHABET = 'a' .. 'z';
our $NON_ALPHABET = '[^' . join("", @ALPHABET) . ']';

sub default_key { 
    #return join "", map { $ALPHABET[int rand @ALPHABET] } for 1..100;
    return 'a';
}

use Class::Tiny qw( key );
sub BUILDARGS { 
    my $class = shift;
    my $key = shift // default_key;
    croak 'ArgumentError' if $key eq '' or $key =~ /$NON_ALPHABET/;
    return {key => $key};
}

sub encode { return $_[0]->_code($_[1], +1) }
sub decode { return $_[0]->_code($_[1], -1) }

sub _code {
    my ($self, $text, $dir) = @_;
    $text =~ s/$NON_ALPHABET//ig;
    $self->key($self->key . $self->key) while length($text) > length($self->key);
    return join "", pairwise 
        { $ALPHABET[ (idx($a) + idx($b) * $dir) % @ALPHABET ] } 
            @{[ split '', lc $text ]}, 
            @{[ split '', substr $self->key, 0, length $text ]};
}

sub idx {
    my $char = shift;
    return firstidx {$char eq $_} @ALPHABET;
}

1;
