import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class BeerSong {
    private static int MAX = 99;

    public String singSong() {
        return sing(MAX, MAX+1);
    }

    public String sing(int start, int count) {
        int stop = start - count + 1;
        return IntStream
                .rangeClosed(stop, start)
                .boxed()
                .sorted(Collections.reverseOrder())
                .map(this::verse)
                .collect(Collectors.joining());
    }

    private String verse(int n) {
        String b = bottle(n);
        String first = String.format("%s on the wall, %s.\n", capitalize(b), b);

        b = bottle(n > 0 ? n - 1 : MAX);
        String second = String.format("%s, %s on the wall.\n\n", task(n), b);

        return first + second;
    }

    private String bottle(int n) {
        return String.format("%s bottle%s of beer",
                n > 0 ? n : "no more",
                n == 1 ? "" : "s");
    }

    private String capitalize(String s) {
        return s.substring(0,1).toUpperCase() + s.substring(1);
    }

    private String task(int n) {
        return n == 0
                ? "Go to the store and buy some more"
                : String.format("Take %s down and pass it around", (n > 1 ? "one" : "it"));
    }
}
