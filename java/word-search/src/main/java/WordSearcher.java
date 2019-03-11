import java.util.*;

public class WordSearcher {

    Map<String, Optional<WordLocation>> search(Set<String> searchWords, char[][] tableau) {
        // initialize the return value
        Map<String, Optional<WordLocation>> foundWords = new HashMap<>();
        for (String word : searchWords) {
            foundWords.put(word, Optional.empty());
        }

        // for each location, look for words in every direction
        for (int row = 0; row < tableau.length; row++) {
            for (int col = 0; col < tableau[0].length; col++) {
                foundWords.putAll(findWordsAt(row, col, searchWords, tableau));
            }
        }

        return foundWords;
    }

    private Map<String, Optional<WordLocation>> findWordsAt(
            int row,
            int col,
            Set<String> searchWords,
            char[][] tableau
    ) {
        Map<String, Optional<WordLocation>> foundWords = new HashMap<>();

        /* starting from the current point, for each direction
         * 1. get the string of characters to the edge of the tablesu
         * 2. iterate through the search words to find any where
         *        tableauString.startsWith(searchWord)
         * 3. determine the end indices.
         */
        int[] deltas = new int[]{-1, 0, 1};

        for (int rowDelta: deltas) {
            for (int colDelta: deltas) {
                if (rowDelta == 0 &&  colDelta == 0)
                    continue;
                String searchLine = getSearchString(tableau, row, col, rowDelta, colDelta);
                foundWords.putAll(findWordsIn(searchLine, searchWords, row, col, rowDelta, colDelta));
            }
        }
        return foundWords;
    }

    private String getSearchString(
            char[][] tableau,
            int row,
            int col,
            int rowDelta,
            int colDelta
    ) {
        StringBuilder sb = new StringBuilder();
        sb.append(tableau[row][col]);
        int r = row + rowDelta;
        int c = col + colDelta;

        while ((0 <= r && r < tableau.length) && (0 <= c && c < tableau[0].length)) {
            sb.append(tableau[r][c]);
            r += rowDelta;
            c += colDelta;
        }

        return sb.toString();
    }

    private Map<String, Optional<WordLocation>> findWordsIn(
            String searchLine,
            Set<String> searchWords,
            int row,
            int col,
            int rowDelta,
            int colDelta
    ) {
        Map<String, Optional<WordLocation>> foundWords = new HashMap<>();
        for (String word : searchWords) {
            if (searchLine.startsWith(word)) {
                foundWords.put(
                        word,
                        getWordLocation(row, col, word.length(), rowDelta, colDelta)
                );
            }
        }
        return foundWords;
    }

    private Optional<WordLocation> getWordLocation(
            int row,
            int col,
            int length,
            int rowDelta,
            int colDelta
    ) {
        Pair start = indicesToPair(row, col);
        Pair end = indicesToPair(
                row + rowDelta * (length - 1),
                col + colDelta * (length - 1)
        );
        return Optional.of(new WordLocation(start, end));
    }

    private Pair indicesToPair(int row, int col) {
        return new Pair(col + 1, row + 1);
    }
}
