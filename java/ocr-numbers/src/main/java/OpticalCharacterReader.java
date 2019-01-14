import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class OpticalCharacterReader {
    private static final int LINES_PER_ROW = 4;
    private static final int COLS_PER_DIGIT = 3;

    private static final List<String> DigitStrings = Arrays.asList(
        " _ | ||_|   ",
        "     |  |   ",
        " _  _||_    ",
        " _  _| _|   ",
        "   |_|  |   ",
        " _ |_  _|   ",
        " _ |_ |_|   ",
        " _   |  |   ",
        " _ |_||_|   ",
        " _ |_| _|   "
    );

    private List<String> rows;

    String parse(List<String> input) {
        rows = input;
        validate();

        return IntStream
                .range(0, input.size() / LINES_PER_ROW)
                .mapToObj(this::getRow)
                .map(this::digitsInRow)
                .collect(Collectors.joining(","));
    }

    // return a 4-line "row" of OCR numbers
    private List<String> getRow(int i) {
        return rows.subList(i * LINES_PER_ROW, (i + 1) * LINES_PER_ROW);
    }

    // for a sublist of 4 lines, determine the digits hiding therein
    private String digitsInRow(List<String> row) {
        int numDigits = row.get(0).length() / COLS_PER_DIGIT;
        return IntStream
                .range(0, numDigits)
                .mapToObj(i -> digitString(row, i))
                .map(DigitStrings::indexOf)
                .map(d -> d == -1 ? "?" : String.valueOf(d))
                .collect(Collectors.joining());
    }

    // Take the vertical slice of substrings for the row of 4 lines
    // and join to form a digit string, an element of DigitStrings array.
    private String digitString(List<String> row, int i) {
        return row
                .stream()
                .map(line -> line.substring(i * COLS_PER_DIGIT, (i + 1) * COLS_PER_DIGIT))
                .collect(Collectors.joining());
    }

    private void validate() {
        if (rows.size() % LINES_PER_ROW != 0)
            throw new IllegalArgumentException(
                "Number of input rows must be a positive multiple of " + LINES_PER_ROW
            );

        if (rows.stream().anyMatch(s -> s.length() % COLS_PER_DIGIT != 0))
            throw new IllegalArgumentException(
                "Number of input columns must be a positive multiple of " + COLS_PER_DIGIT
            );
    }
}
