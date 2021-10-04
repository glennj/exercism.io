package Say;
use strictures 2;
use Exporter::Easiest 'OK => say_number';
use Carp;

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

sub say_number {
    my $n = shift;
    if ($n < 0)    { croak 'input out of range' }
    if ($n < 100)  { return $SMALL[$n] 
                         || $XTY{$n}
                         || say_number($n - $n % 10) . '-' . say_number($n % 10) }
    if ($n < 1000) { return say_compound($n, 100, 'hundred') }
    if ($n < 1e6)  { return say_compound($n, 1e3, 'thousand') }
    if ($n < 1e9)  { return say_compound($n, 1e6, 'million') }
    if ($n < 1e12) { return say_compound($n, 1e9, 'billion') }
    croak 'input out of range';
}

sub say_compound {
    my ($n, $base, $word) = @_;
    my $rem = $n % $base;
    return join ' ', grep {$_} (
        say_number(($n - $rem) / $base),
        $word,
        $rem && say_number($rem)
    );
}

1;
