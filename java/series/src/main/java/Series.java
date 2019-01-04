import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Series {
    private final String source;

    Series(String input) {
        source = input;
    }

    List<String> slices(int size) throws IllegalArgumentException {
        if (size > source.length())
            throw new IllegalArgumentException("Slice size is too big.");
        if (size < 1)
            throw new IllegalArgumentException("Slice size is too small.");

        return IntStream
                .rangeClosed(0, source.length() - size)
                .mapToObj(i -> source.substring(i, i + size))
                .collect(Collectors.toList());
    }
}
