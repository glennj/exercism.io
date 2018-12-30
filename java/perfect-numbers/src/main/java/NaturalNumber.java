import java.util.function.IntPredicate;
import java.util.stream.IntStream;

class NaturalNumber {
    private final int number;
    private final int aliquotSum;

    NaturalNumber(int number) {
        if (number <= 0)
            throw new IllegalArgumentException(
                "You must supply a natural number (positive integer)"
            );

        this.number = number;
        this.aliquotSum = aliquotSum();
    }

    int getNumber()     { return number; }
    int getAliquotSum() { return aliquotSum; }

    Classification getClassification() {
        if (aliquotSum > number) return Classification.ABUNDANT;
        if (aliquotSum < number) return Classification.DEFICIENT;
        return Classification.PERFECT;
    }

    private int aliquotSum() {
        if (number == 1) return 0;

        int sqrt = (int) Math.floor(Math.sqrt(number));
        IntPredicate isFactor = i -> (number % i) == 0;

        return IntStream
            .rangeClosed(1, sqrt)
            .filter(isFactor)
            .map(f -> sumOfFactors(f))
            .sum();
    }

    private int sumOfFactors(int factor) {
        // If the factor _is_ the number's square root, only count it once.
        // If the factor is 1, don't include the number
        int other = (factor == 1 || factor * factor == number)
            ? 0
            : number / factor;
        return factor + other;
    }
}
