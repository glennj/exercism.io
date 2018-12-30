import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class LargestSeriesProductCalculator {
    private final List<Integer> digits;

    LargestSeriesProductCalculator(String inputNumber) throws IllegalArgumentException {
        if (inputNumber.matches(".*\\D.*"))
            throw new IllegalArgumentException("String to search may only contain digits.");

        digits = inputNumber.chars()
                .mapToObj(c -> Character.getNumericValue((char) c))
                .collect(Collectors.toList());
    }

    long calculateLargestProductForSeriesLength(int numberOfDigits) throws IllegalArgumentException {
        if (numberOfDigits > digits.size())
            throw new IllegalArgumentException(
                "Series length must be less than or equal to the length of the string to search."
            );
        if (numberOfDigits < 0)
            throw new IllegalArgumentException("Series length must be non-negative.");

        return IntStream
                .rangeClosed(0, digits.size() - numberOfDigits)
                .mapToLong(i -> product(i, numberOfDigits))
                .max()
                .orElse(0L);
    }

    private long product(int i, int numberOfDigits) {
        return digits
                .subList(i, i + numberOfDigits)
                .stream()
                .mapToLong(Integer::longValue)
                .reduce(1L, (p, n) -> p * n);
    }
}
