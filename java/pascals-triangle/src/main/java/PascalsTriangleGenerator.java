import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

public class PascalsTriangleGenerator {
    public int[][] generateTriangle(int size) throws IllegalArgumentException {
        return new Streamed().generateTriangle(size);
    }

    public static class Streamed {
        public int[][] generateTriangle(int size) throws IllegalArgumentException {
            if (size < 0)
                throw new IllegalArgumentException();

            return IntStream
                    .range(0, size)
                    .mapToObj(this::generateRow)
                    .toArray(int[][]::new);
        }

        private int[] generateRow(int n) {
            return IntStream
                    .rangeClosed(0, n)
                    .map(k -> binomCoeff(n, k))
                    .toArray();
        }

        private int binomCoeff(int n, int k) {
            return fact(n) / (fact(k) * fact(n - k));
        }

        private static Map<Integer, Integer> factCache = new HashMap<>();
        static { factCache.put(0, 1); }

        private int fact(int n) {
            if (!factCache.containsKey(n)) {
                int f = IntStream.rangeClosed(1, n).reduce(1, (a, b) -> a * b);
                factCache.put(n, f);
            }
            return factCache.get(n);
        }
    }

    public static class Iterative {
        public int[][] generateTriangle(int size) throws IllegalArgumentException {
            if (size < 0)
                throw new IllegalArgumentException();

            int[][] triangle = new int[size][size];
            for (int n = 0; n < size; n++) {
                triangle[n] = new int[n + 1];
                for (int k = 0; k <= n; k++) {
                    triangle[n][k] = binomialCoeff(n, k);
                }
            }
            return triangle;
        }

        // binomial coefficient, or "n choose k"
        private int binomialCoeff(int n, int k) {
            if (k == 0 || n - k == 0) return 1;
            if (k == 1 || n - k == 1) return n;
            return fact(n) / (fact(k) * fact(n - k));
        }

        // factorial, cached
        private static final Map<Integer, Integer> factCache = Collections.synchronizedMap(new HashMap<>());

        private int fact(int n) {
            if (factCache.containsKey(n)) return factCache.get(n);
            if (n <= 1) return 1;
            int f = n * fact(n - 1);
            factCache.put(n, f);
            return f;
        }
    }
}
