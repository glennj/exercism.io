package Prime;
use strictures 2;

sub prime_factor_generator;
sub prime_number_generator;

sub factors {
    my @factors;
    my $pfg = prime_factor_generator shift;
    while (defined(my $f = $pfg->())) {
        push @factors, $f;
    }
    return \@factors;
}

sub prime_factor_generator {
    my $n = shift;
    my $next_prime = prime_number_generator;
    my $f = $next_prime->();

    return sub {
        NEXT_PRIME:
        if ($f * $f <= $n) {
            if ($n % $f == 0) {
                $n /= $f;
                return $f;
            } else {
                $f = $next_prime->();
                goto NEXT_PRIME;
            }
        } else {
            if ($n > 1) {
                my $final = $n;
                $n = 1;
                return $final;
            }
            return;
        }
    };
}

sub prime_number_generator {
    my ($current) = @_;
    $current //= 2;
    my @primes;

    return sub {
        if ($current == 2) {
            push @primes, 2;
            $current = 3;
            return 2;
        }
        CHECK_PRIME:
        for my $p (@primes) {
            last if $p * $p > $current;
            if ($current % $p == 0) {
                $current += 2;
                goto CHECK_PRIME;
            }
        }
        my $prime = $current;
        $current += 2;
        push @primes, $prime;
        return $prime;
    };
}

1;

__END__
# simple iterative approach
sub factors {
    my $n = shift;
    my @factors;
    my $f = 2;
    while ($f * $f <= $n) {
        if ($n % $f == 0) {
            push @factors, $f;
            $n /= $f;
        } else {
            $f += $f == 2 ? 1 : 2;
        }
    }
    push @factors, $n if $n > 1;
    return \@factors;
}

1;
