package SimpleCipher;

our @ALPHABET = 'a' .. 'z';

use v5.38;
use List::Util qw/ pairmap mesh /;
use Moo;

has key => (
    is => 'lazy',
);

sub encode ($self, $plaintext) {
    return $self->_encipher($plaintext, 1);
}

sub decode ($self, $ciphertext) {
    return $self->_encipher($ciphertext, -1);
}

sub _build_key ($self) {
    return join '', map {$ALPHABET[int rand @ALPHABET]} 1..100;
}

sub _encipher($self, $text, $direction) {
    # fit key to text
    my $key = $self->key;
    while (length($key) < length($text)) {
        $key .= $key;
    }
    $key = substr $key, 0, length $text;

    return join '',
        pairmap { $ALPHABET[ (idx($a) + idx($b) * $direction) % @ALPHABET ] } 
        mesh [ split '', $text ], [ split '', $key ];
}

sub idx($char) {
    for my $i (0 .. $#ALPHABET) {
        return $i if $ALPHABET[$i] eq $char;
    }
    return -1;
}

