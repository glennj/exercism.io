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
