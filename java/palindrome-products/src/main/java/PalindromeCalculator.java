import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.IntStream;

class PalindromeCalculator {

    SortedMap<Long, List<List<Integer>>> getPalindromeProductsWithFactors(int min, int max) {
        if (min > max)
            throw new IllegalArgumentException("invalid input: min must be <= max");

        SortedMap<Long, List<List<Integer>>> palProducts = new TreeMap<>();

        IntStream
            .rangeClosed(min, max)
            .forEach(i -> {
                IntStream
                    .rangeClosed(i, max)
                    .filter(j -> isPalindrome(i * j))
                    .forEach(j -> {
                        List<List<Integer>> factList = palProducts.computeIfAbsent(
                                (long)(i * j),
                                key -> new ArrayList<>()
                        );
                        factList.add(Arrays.asList(i, j));
                    });
            });

        return palProducts;
    }

    private boolean isPalindrome(long n) {
        return n == palindromeOf(n);
    }

    private long palindromeOf(long n) {
        long pal = 0;
        while (n > 0) {
            pal = pal * 10 + n % 10;
            n /= 10;
        }
        return pal;
    }
}
