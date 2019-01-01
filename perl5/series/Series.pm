package Series;
use strictures 2;
use Carp;

sub new {
    my ($class, $input) = @_;
    croak 'ArgumentError' if $input =~ /\D/;
    return bless \$input, $class;
}

sub slice {
    my ($self, $size) = @_;
    my $len = length $$self;
    croak 'ArgumentError' unless 1 <= $size and $size <= $len;
    return [
        map { [split ''] }
        map { substr($$self, $_, $size) }
        0 .. ($len - $size)
    ];
}

1;
