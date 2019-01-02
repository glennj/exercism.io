import java.util.LinkedList;
import java.util.function.IntPredicate;
import java.util.function.IntSupplier;
import java.util.stream.IntStream;

class PrimeCalculator {

    // memoize the generated primes.
    private static LinkedList<Integer> primes = new LinkedList<>();
    static {
        // seed the list with the first prime
        primes.add(2);
    }

    private static IntSupplier primeNumbers = new IntSupplier() {
        // using previous primes, determine if this number is prime
        IntPredicate isPrime = n -> {
            int sqrt = (int) Math.floor(Math.sqrt(n));
            for (int p : primes) {
                if (p > sqrt) break;
                if (n % p == 0) return false;
            }
            return true;
        };

        // next prime
        @Override
        public int getAsInt() {
            int current = primes.getLast();
            int next = current;
            if (current == 2) {
                next = 3;
            }
            else {
                do { next += 2; } while (!isPrime.test(next));
            }
            primes.add(next);
            return current;
        }
    };

    int nth(int nth) {
        if (nth < 1) throw new IllegalArgumentException();

        // Do we already have the requested prime?
        int primesToCalculate = nth - PrimeCalculator.primes.size() + 1;
        if (primesToCalculate < 1)
            return PrimeCalculator.primes.get(nth - 1);

        // the reducer simply remembers the last stream element
        return IntStream
                .generate(PrimeCalculator.primeNumbers)
                .limit(primesToCalculate)
                .reduce(-1, (prime, p) -> p);
    }
}
