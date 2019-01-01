import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class DiamondPrinter {

    private static final char BASE = 'A';

    List<String> printToList(char a) {
        int size = a - BASE + 1;

        // create the top right quadrant
        char[][] quadrant = new char[size][size];
        for (int i = 0; i < size; i++) {
            Arrays.fill(quadrant[i], ' ');
            quadrant[i][i] = (char)(BASE + i);
        }

        // create the top half
        List<String> diamond = Arrays
                .stream(quadrant)
                .map(String::new)
                .map(s -> new StringBuilder(s).deleteCharAt(0).reverse().append(s))
                .map(StringBuilder::toString)
                .collect(Collectors.toList());

        // add the bottom half
        for (int i = size - 2; i >= 0; i--) {
            diamond.add(diamond.get(i));
        }

        return diamond;
    }

}
