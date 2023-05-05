# Returns the prime numbers less than or equal to the given limit.
#
# + 'limit - as an int
# + return - prime numbers as an array of int
public function primes(int 'limit) returns int[] {
    boolean[] candidates = [false, false]; 
    foreach int _ in int:range(2, 'limit + 1, 1) {
        candidates.push(true);
    }

    var markMultiples = function(int n) {
        foreach int m in int:range(n * n, 'limit + 1, n == 2 ? 2 : n * 2) {
            candidates[m] = false;
        }
    };

    int sqr = <int>((<float>'limit).sqrt().floor());
    markMultiples(2);
    foreach int n in int:range(3, sqr + 1, 2) {
        if candidates[n] {
            markMultiples(n);
        }
    }

    return from [int, boolean] [i, isPrime] in candidates.enumerate()
           where isPrime
           select i;
}
