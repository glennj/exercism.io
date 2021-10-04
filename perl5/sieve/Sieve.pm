package Sieve;

use strictures 2;
use Exporter::Easiest 'OK => find_primes';

sub find_primes {
    my ($limit) = @_;
    return __PACKAGE__->new($limit)->primes;
}

use Moo;
use Set::Tiny;

has limit => (
    is => 'ro',
);
sub BUILDARGS { return { limit => pop } }

has candidates => (
    is => 'lazy',
    default => sub {
        my $self = shift;
        return Set::Tiny->new( 2 .. $self->limit );
    },
);

has primes => (
    is => 'lazy',
    default => sub {
        my $self = shift;
        $self->eratosthenes;
        return [ sort {$a <=> $b} $self->candidates->elements ];
    },
);

sub eratosthenes {
    my $self = shift;
    $self->_remove_multiples(2);
    my $upto = int(sqrt($self->limit));
    for (my $i = 3; $i <= $upto; $i += 2) {
        next if sqrt($i) == int(sqrt($i));
        $self->_remove_multiples($i);
    }
    return $self;
}

sub _remove_multiples {
    my ($self, $n) = @_;
    my $inc = $n == 2 ? 2 : $n * 2;
    for (my $i = $n * $n; $i <= $self->limit; $i += $inc) {
        $self->candidates->delete($i);
    }
    return;
}

1;
