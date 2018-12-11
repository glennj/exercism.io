package Say;
use strictures 2;
use Carp;

sub new { return bless {english => Say(pop)}, shift }
sub in_english { return shift->{english} }

# translation of this lovely recursive solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

our @SMALL = (
    'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
    'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen',
    'sixteen', 'seventeen', 'eighteen', 'nineteen'
);

our %XTY = (
    20 => 'twenty', 30 => 'thirty', 40 => 'forty', 50 => 'fifty',
    60 => 'sixty', 70 => 'seventy', 80 => 'eighty', 90 => 'ninety'
);

sub Say {
    my $n = shift;
    if ($n < 0)    { croak 'ArgumentError' }
    if ($n < 100)  { return $SMALL[$n] 
                         || $XTY{$n}
                         || Say($n - $n % 10) . '-' . Say($n % 10) }
    if ($n < 1000) { return SayCompound($n, 100, 'hundred') }
    if ($n < 1e6)  { return SayCompound($n, 1e3, 'thousand') }
    if ($n < 1e9)  { return SayCompound($n, 1e6, 'million') }
    if ($n < 1e12) { return SayCompound($n, 1e9, 'billion') }
    croak 'ArgumentError';
}

sub SayCompound {
    my ($n, $base, $word) = @_;
    my $rem = $n % $base;
    return join ' ', grep {$_} (
        Say(($n - $rem) / $base),
        $word,
        $rem && Say($rem)
    );
}

1;
