import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class TwelveDays {

    private static final String[] GIFTS = {
        "a Partridge in a Pear Tree", "two Turtle Doves",
        "three French Hens", "four Calling Birds",
        "five Gold Rings", "six Geese-a-Laying",
        "seven Swans-a-Swimming", "eight Maids-a-Milking",
        "nine Ladies Dancing", "ten Lords-a-Leaping",
        "eleven Pipers Piping", "twelve Drummers Drumming"
    };

    private static final String[] ORDINALS = {
        "first", "second", "third", "fourth", "fifth", "sixth",
        "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"
    };

    String sing() {
        return verses(1, GIFTS.length);
    }

    String verses(int startVerse, int endVerse) {
        return IntStream
                .rangeClosed(startVerse, endVerse)
                .mapToObj(this::verse)
                .collect(Collectors.joining("\n"));
    }

    String verse(int verseNumber) {
        List<String> gifts = new ArrayList<>();
        for (int i = verseNumber - 1; i >= 1; i--)
            gifts.add(GIFTS[i]);

        return String.format("On the %s day of Christmas my true love gave to me: %s%s%s.\n",
                ORDINALS[verseNumber - 1],
                gifts.stream().collect(Collectors.joining(", ")),
                verseNumber > 1 ? ", and " : "",
                GIFTS[0]
        );
    }
}
