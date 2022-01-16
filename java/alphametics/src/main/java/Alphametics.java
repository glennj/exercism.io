import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.Pattern;

/* Solving Alphametics brute force.
 * Performance is terrible: 
 * - the 8 letter puzzle required about 1 min to run;
 */
class Alphametics {
    private final Character[] letters;
    private final String[] words;

    Alphametics(String puzzle) {
        letters = puzzle
                .chars()
                .filter(Character::isUpperCase)
                .distinct()
                .sorted()
                .mapToObj(c -> (char) c)
                .toArray(Character[]::new);

        words = puzzle.split("[^\\p{Upper}]+");
    }

    Map<Character, Integer> solve() throws UnsolvablePuzzleException {
        if (letters.length > 10)
            throw new UnsolvablePuzzleException();

        for (Map<Character, Integer> mapping : new Permutator()) {
            if (mappingSolvesPuzzle(mapping))
                return mapping;
        }
        throw new UnsolvablePuzzleException();
    }

    private boolean mappingSolvesPuzzle(Map<Character, Integer> mapping) {
        long sum = 0;
        long value = 0;
        for (String word : words) {
            String asDigits = digitize(word, mapping);
            if (asDigits.startsWith("0"))
                return false;
            value = Long.parseLong(asDigits);
            sum += value;
        }
        // Here, value is the value of the last word (the equation's solution)
        // It has been included in the sum already.
        return (sum - value) == value;
    }

    /* Given the char to int mapping, transform a word of characters into a
     * word of digits.
     */
    private String digitize(String word, Map<Character, Integer> mapping) {
        StringBuilder sb = new StringBuilder();
        word.chars().forEach(c -> sb.append(mapping.get((char) c)));
        return sb.toString();
    }

    /* Brute force: start with mapping A=>0, B=>1, C=>2, ...
     * ending with A=>9, B=>8, C=>7, ...
     */
    private class Permutator implements Iterable<Map<Character, Integer>> {
        @Override
        public Iterator<Map<Character, Integer>> iterator() {
            return new Iterator<Map<Character, Integer>>() {
                long current = Long.parseLong("0123456789".substring(0, letters.length), 10);
                long end     = Long.parseLong("9876543210".substring(0, letters.length), 10);
                String padNumber = "%0" + letters.length + "d";
                Pattern repeatedDigit = Pattern.compile("(.).*\\1");

                @Override
                public boolean hasNext() {
                    return current <= end;
                }

                @Override
                public Map<Character, Integer> next() {
                    String digitString = nextDigitString();
                    if (current > end) return null;
                    return createMapping(digitString);
                }

                private String nextDigitString() {
                    String digitString;
                    while (true) {
                        digitString = String.format(padNumber, current);
                        if (!repeatedDigit.matcher(digitString).find())
                            break;
                        current++;
                    }
                    return digitString;
                }

                private Map<Character, Integer> createMapping(String digitString) {
                    Map<Character, Integer> mapping = new HashMap<>();
                    for (int i = 0; i < letters.length; i++) {
                        mapping.put(
                                letters[i],
                                Character.getNumericValue(digitString.charAt(i))
                        );
                    }
                    current++;
                    return mapping;
                }
            };
        }
    }
}
