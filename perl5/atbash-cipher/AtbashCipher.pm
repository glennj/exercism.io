#!perl
## no critic (Subroutines::RequireFinalReturn)
## no critic (RegularExpressions::RequireExtendedFormatting)

package AtbashCipher;
use strictures 2;
use Exporter::Easiest 'OK => encode_atbash decode_atbash';

our %CODE;
@CODE{'a'..'z'} = reverse 'a'..'z';
@CODE{0..9} = 0..9;

sub group5 { join ' ', (shift =~ /.{1,5}/g) }
sub code   { join '', map {$CODE{+lc}} (shift =~ /[[:alnum:]]/g) }

sub encode_atbash { group5 code shift }
sub decode_atbash {        code shift }

1;
