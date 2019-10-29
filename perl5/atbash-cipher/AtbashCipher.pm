package AtbashCipher;
use strictures 2;
use Exporter 'import';
our @EXPORT_OK = qw(encode_atbash decode_atbash);

use List::MoreUtils qw/ pairwise /;

our %CODE = pairwise {$a => $b} @{['a'..'z']}, @{[reverse 'a'..'z']};
$CODE{$_} = $_ for 0..9;

sub group5 { join ' ', (shift =~ /.{1,5}/g) }
sub code   { join '', map {$CODE{+lc}} (shift =~ /[[:alnum:]]/g) }

sub encode_atbash { group5 code shift }
sub decode_atbash {        code shift }

1;
