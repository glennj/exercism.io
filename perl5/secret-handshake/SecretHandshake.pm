package SecretHandshake;
use strictures 2;
use List::Util qw/ pairgrep pairvalues /;

our @ACTS = ( 
    0b0001 => 'wink',
    0b0010 => 'double blink',
    0b0100 => 'close your eyes',
    0b1000 => 'jump',
);
our $REVERSE = 0b10000;

sub new {
    my ($class, $input) = @_;
    return bless \$input, $class;
}

sub commands {
    my $self = shift;
    return [] if $$self =~ /\D/;
    my $actions = [ pairvalues pairgrep { $$self & $a } @ACTS ];
    return ($$self & $REVERSE) ? [reverse @$actions] : $actions;
}

1;
