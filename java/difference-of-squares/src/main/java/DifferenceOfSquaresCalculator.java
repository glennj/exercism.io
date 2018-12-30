import java.util.stream.IntStream;

class DifferenceOfSquaresCalculator {

    int computeSquareOfSumTo(int input) {
        return (int) Math.pow(IntStream.rangeClosed(1, input).sum(), 2);
    }

    int computeSumOfSquaresTo(int input) {
        return IntStream.rangeClosed(1, input)
            .map(i -> i * i)
            .sum();
    }

    int computeDifferenceOfSquares(int input) {
        return computeSquareOfSumTo(input) - computeSumOfSquaresTo(input);
    }
}

/*
https://exercism.io/tracks/java/exercises/difference-of-squares/solutions/1e91f9c092b743e5bdbedc3daaea0803
*/
