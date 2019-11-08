import java.util.Arrays;
import java.util.List;

class ResistorColorDuo {
    static final List<String> COLORS = Arrays.asList(
            "black", "brown", "red", "orange", "yellow",
            "green", "blue", "violet", "grey", "white"
    );


    int value(String[] colors) {
        // test suite does not include:
        // - not enough colors
        // - invalid color string

        int c1 = COLORS.indexOf(colors[0]);
        int c2 = COLORS.indexOf(colors[1]);
        return 10 * c1 + c2;
    }
}

// TODO a stream version