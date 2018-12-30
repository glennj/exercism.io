import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.Collectors;

class Proverb {

    private static final String LINE1 = "For want of a %s the %s was lost.\n";
    private static final String LINE2 = "And all for the want of a %s.";
    private final String[] words;

    Proverb(String[] words) {
        this.words = words;
    }

    String recite() {
        return recite_streamed();
    }

    String recite_streamed() {
        if (words.length == 0) return "";

        Stream<String> these = Arrays.stream(
            Arrays.copyOf(words, words.length - 1)
        );
        Iterator<String> those = Arrays.stream(
            Arrays.copyOfRange(words, 1, words.length)
        ).iterator();

        String proverb = these
            .map(item -> String.format(LINE1, item, those.next()))
            .collect(Collectors.joining());

        return proverb + String.format(LINE2, words[0]);
    }

    String recite_iterative() {
        StringBuilder sb = new StringBuilder();

        for (int i = 1; i < words.length; i++)
            sb.append(String.format(LINE1, words[i - 1], words[i]));

        if (words.length > 0)
            sb.append(String.format(LINE2, words[0]));            

        return sb.toString();
    }
}
