import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class NucleotideCounter {

    private static final String VALID_NUCLEOTIDES = "ACGT";
    private Map<Character, Integer> count;

    NucleotideCounter(String strand) {
        count = VALID_NUCLEOTIDES
                .chars()
                .mapToObj(c -> (char) c)
                .collect(Collectors.toMap(
                        key -> key,
                        value -> 0
                ));

        if (!strand.matches("^[" + VALID_NUCLEOTIDES + "]*$"))
            throw new IllegalArgumentException();

        strand.chars().forEach(c -> count.compute((char) c, (k, v) -> v + 1));
    }

    public Map<Character, Integer> nucleotideCounts() {
        return count;
    }
}
