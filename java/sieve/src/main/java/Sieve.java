import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Sieve {
    private final int max;
    private final List<Integer> primes;

    Sieve(int maxPrime) {
        max = maxPrime;
        primes = eratosthenes();
    }

    List<Integer> getPrimes() {
        return primes;
    }

    private List<Integer> eratosthenes() {
        List<Integer> numbers = IntStream
                .rangeClosed(0, max)
                .mapToObj(Integer::new)
                .collect(Collectors.toList());

        removeMultiples(numbers, 2);
        
        int sqrt = (int) Math.floor(Math.sqrt(max));
        for (int i = 3; i <= sqrt; i += 2)
            removeMultiples(numbers, i);

        return numbers
                .stream()
                .filter(i -> i != null && i >= 2)
                .collect(Collectors.toList());
    }

    private void removeMultiples(List<Integer> numbers, int n) {
        int step = 2 * (n == 2 ? 1 : n);
        for (int i = n * n; i <= max; i += step) {
            numbers.set(i, null);
        }
    }
}
