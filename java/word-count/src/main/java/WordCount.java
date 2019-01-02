import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

public class WordCount {
    public Map<String, Integer> phrase(String phrase) {
        return Arrays
                .stream(phrase
                        .toLowerCase()
                        .replaceAll("[^'\\p{Alnum}]+", " ")
                        .trim()
                        .split("\\s+")
                )
                .map(word ->
                        word.replaceFirst("^'(.+)'$", "$1")
                )
                .collect(Collectors.toMap(
                        word -> word,
                        value -> 1,
                        Integer::sum
                )
        );
    }

//    public Map<String, Integer> phrase(String phrase) {
//        Map<String, Integer> count = new HashMap<>();
//
//        String[] words = phrase
//                .toLowerCase()
//                .replaceAll("[^'\\p{Alnum}]+", " ")
//                .trim()
//                .split("\\s+");
//
//        Arrays.stream(words)
//                .map(word -> word.replaceFirst("^'(.+)'$", "$1"))
//                .forEach(word -> count.compute(word, (k, v) -> v == null ? 1 : v + 1));
//
//        return count;
//    }
}
