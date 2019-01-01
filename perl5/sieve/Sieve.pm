package Sieve;
use strictures 2;
use Set::Tiny;
use Class::Tiny qw/ limit primes /;

sub BUILDARGS { return { limit => pop } }
sub BUILD     { shift->eratosthenes }

sub eratosthenes {
    my $self = shift;
    $self->primes( Set::Tiny->new( 2 .. $self->limit ) );

    $self->remove_multiples(2);

    my $upto = int(sqrt($self->limit));
    for (my $i = 3; $i <= $upto; $i += 2) {
        next if sqrt($i) == int(sqrt($i));
        $self->remove_multiples($i);
    }

    $self->primes( [ sort {$a <=> $b} $self->primes->elements ] );
}

sub remove_multiples {
    my ($self, $n) = @_;
    my $inc = $n == 2 ? 2 : $n * 2;
    for (my $i = $n * $n; $i <= $self->limit; $i += $inc) {
        $self->primes->delete($i);
    }
}

1;
