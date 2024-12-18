package Pangram;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<is_pangram>;

sub char_index { ( ord lc shift ) - ord 'a' }

sub is_pangram ($text) {
    my $bitfield = 0;
    $bitfield |= ( 1 << char_index($_) ) for ( $text =~ /[a-z]/gi );
    return $bitfield == 0b11_1111_1111_1111_1111_1111_1111;
}

1;
