import java.util.Map;
import java.util.HashMap;

class Scrabble {

    private static final Map<Character, Integer> TILES;
    
    static void addTiles(String letters, int value) {
        letters.chars().forEach(c -> TILES.put((char) c, value));
    }
    
    static {
        TILES = new HashMap<>();
        addTiles("AEIOULNRST", 1);
        addTiles("DG", 2);
        addTiles("BCMP", 3);
        addTiles("FHVWY", 4);
        addTiles("K", 5);
        addTiles("JX", 8);
        addTiles("QZ", 10);
    }

    private final int score;

    Scrabble(String word) {
        if (word.matches("[^\\p{Alpha}]"))
            throw new IllegalArgumentException("word contains non-letters.");

        this.score = word
            .toUpperCase()
            .chars()
            .map(c -> TILES.get((char) c))
            .sum();
    }

    int getScore() {
        return score;
    }

}
