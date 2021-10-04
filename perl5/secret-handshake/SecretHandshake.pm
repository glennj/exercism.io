package SecretHandshake;

use strictures 2;
use Exporter::Easiest 'OK => handshake';
use List::Util qw/ pairgrep pairvalues /;

our @ACTS = ( 
    0b0001 => 'wink',
    0b0010 => 'double blink',
    0b0100 => 'close your eyes',
    0b1000 => 'jump',
);
our $REVERSE = 0b10000;

sub handshake {
    my $num = shift;
    return [] if $num =~ /\D/;
    my $actions = [ pairvalues pairgrep { $num & $a } @ACTS ];
    return ($num & $REVERSE) ? [reverse @$actions] : $actions;
}

1;
