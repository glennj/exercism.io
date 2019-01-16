import java.util.Map;
import java.util.stream.Collectors;

class ParallelLetterFrequency {
    private final String text;

    ParallelLetterFrequency(String input) {
        text = input;
    }

    // https://stackoverflow.com/a/29127257/7552
    Map<Integer, Integer> letterCounts() {
        return text
                .chars()
                .parallel()
                .filter(Character::isAlphabetic)
                .map(Character::toLowerCase)
                .boxed()
                .collect(Collectors.toConcurrentMap(
                        key -> key,
                        val -> 1,
                        Integer::sum
                ));
    }
}
