import java.util.List;
import java.util.stream.Collectors;

class Anagram {
    private final String anagram;
    private final String original;

    Anagram(String word) {
        anagram = toAnagram(word);
        original = word;
    }

    private String toAnagram(String word) {
        return word
                .toLowerCase()
                .chars()
                .mapToObj(c -> (char) c)
                .sorted()
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }

    List<String> match(List<String> words) {
        return words
                .stream()
                .filter(w -> !w.equalsIgnoreCase(original))
                .filter(w -> toAnagram(w).equals(anagram))
                .collect(Collectors.toList());
    }
}
