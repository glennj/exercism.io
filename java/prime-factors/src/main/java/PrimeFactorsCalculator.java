import java.util.ArrayList;
import java.util.List;

class PrimeFactorsCalculator {

    // from the "Nth Prime" exercise
    private PrimeCalculator primes = new PrimeCalculator();
    private int nth = 0;

    private long nextPrime() {
        nth += 1;
        return (long) primes.nth(nth);
    }

    public List<Long> calculatePrimeFactorsOf(long number) {
        List<Long> factors = new ArrayList<>();
        long factor = nextPrime();

        while (factor * factor <= number) {
            if (number % factor == 0) {
                factors.add(factor);
                number /= factor;
            }
            else {
                factor = nextPrime();
            }
        }
        if (number > 1) factors.add(number);
        return factors;
    }
}