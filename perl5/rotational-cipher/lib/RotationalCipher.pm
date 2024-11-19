package RotationalCipher;

## no critic (ValuesAndExpressions::ProhibitVersionStrings, Subroutines::ProhibitSubroutinePrototypes)

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<caesar_cipher>;

sub caesar_cipher ( $text, $shift_key ) {
    my $alphabet = join '', 'a' .. 'z';
    my $rotated = substr( $alphabet, $shift_key ) . substr( $alphabet, 0, $shift_key );
    $alphabet .= uc $alphabet;
    $rotated  .= uc $rotated;

    # ref https://perldoc.pl/perlop#tr/SEARCHLIST/REPLACEMENTLIST/cdsr
    # > Because the transliteration table is built at compile time, neither
    # > the SEARCHLIST nor the REPLACEMENTLIST are subjected to double quote
    # > interpolation. That means that if you want to use variables, you must
    # > use an eval():

    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    return eval sprintf( '$text =~ tr/%s/%s/r', $alphabet, $rotated );
}

1;
