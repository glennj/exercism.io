import java.util.HashSet;
import java.util.Set;
import java.util.Optional;

public class PangramChecker {
    public boolean isPangram(String input) {
        Set<Character> chars = new HashSet<>();
        Optional.ofNullable(input).orElse("")
            .toString()
            .toLowerCase()
            .replaceAll("[^a-z]", "")
            .chars()
            .mapToObj(c -> Character.valueOf((char) c))
            .forEach(c -> chars.add(c));
        return chars.size() == 26;
    }
}

/* community

https://exercism.io/tracks/java/exercises/pangram/solutions/c664ed2438a54f30b284128d1b69e112

    public boolean isPangram(String input) {
        return input.toLowerCase().chars()
                .filter(Character::isAlphabetic)
                .distinct()
                .count() == LETTERS_IN_ALPHABET;
    }
 */
