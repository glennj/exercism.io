module nth_prime;

import std.exception : enforce;

// "prime" the pump. ahem.
private int[] primes = [2, 3, 5];

@safe int prime(immutable int number) { // not pure due to mutable primes array
    enforce(number > 0);

    bool isPrime(int n) {
        foreach (p; primes) {
            if (p * p > n) break;
            if (n % p == 0) return false;
        }
        return true;
    }

    int nextPrime() {
        int p = primes[$ - 1];
        do { p += 2; } while (!isPrime(p));
        return p;
    }

    while (primes.length < number)
        primes ~= nextPrime();

    return primes[number - 1];
}


unittest
{
    import std.exception : assertThrown;

    immutable int allTestsEnabled = 1;

    // First prime
    assert(prime(1) == 2);

    static if (allTestsEnabled)
    {
        // Second prime
        assert(prime(2) == 3);

        // Sixth prime
        assert(prime(6) == 13);

        // Big prime
        assert(prime(10_001) == 10_4743);

        // There is no zeroth prime
        assertThrown(prime(0));
    }

}
