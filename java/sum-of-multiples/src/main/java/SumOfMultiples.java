import java.util.Arrays;
import java.util.function.IntPredicate;
import java.util.stream.IntStream;

class SumOfMultiples {

    private int sum;

    SumOfMultiples(int number, int[] set) {
        
        IntPredicate hasFactor = n -> Arrays
            .stream(set)
            .anyMatch(f -> f > 0 && n % f == 0);

        sum = IntStream
            .range(1, number)
            .filter(hasFactor)
            .sum();
    }

    int getSum() {
        return sum;
    }
}
