import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class RailFenceCipher {

    /** A RailWalker knows how to walk the rails, reversing direction
     *  when it hits one of the the outer rails.
     */
    private static class RailWalker {
        private int rail = -1;
        private int inc = 1;
        private int numRails;
        RailWalker(int rails) { numRails = rails; }
        int next() {
            if ((rail == 0 && inc < 0) || (rail == numRails - 1 && inc > 0))
                inc *= -1;
            rail += inc;
            return rail;
        }
    }

    private int numRails;

    RailFenceCipher(int rails) {
        numRails = rails;
    }

    /** Place each character of the plaintext on the "next" rail,
     *  then concatenate the rail text.
     */
    String getEncryptedData(String plaintext) {
        if (numRails == 1 || numRails > plaintext.length())
            return plaintext;

        List<StringBuilder> rails = IntStream
                .range(0, numRails)
                .mapToObj(i -> new StringBuilder())
                .collect(Collectors.toList());

        RailWalker rw = new RailWalker(numRails);
        plaintext.chars().forEach(c -> rails.get(rw.next()).append((char) c));

        return rails.stream()
                .map(StringBuilder::toString)
                .collect(Collectors.joining());
    }

    /** Split the ciphertext into what would appear on the rails,
     *  then iteratively select the next character from each rail
     *  and join the characters to form the plain text.
     */
    String getDecryptedData(String ciphertext) {
        if (numRails == 1 || numRails > ciphertext.length())
            return ciphertext;

        List<StringBuilder> rails = partition(ciphertext);
        RailWalker rw = new RailWalker(numRails);

        return IntStream
                .rangeClosed(1, ciphertext.length())
                .mapToObj(i -> {
                    StringBuilder rail = rails.get(rw.next());
                    char c = rail.charAt(0);
                    rail.deleteCharAt(0);
                    return c;
                })
                .reduce(new StringBuilder(), StringBuilder::append, StringBuilder::append)
                .toString();
    }

    /** Split the ciphertext into rails.
     */
    private List<StringBuilder> partition(String ciphertext) {
        AtomicInteger i = new AtomicInteger(0);
        return partitionLengths(ciphertext)
                .stream()
                .map(len -> ciphertext.substring(i.get(), i.addAndGet(len)))
                .map(StringBuilder::new)
                .collect(Collectors.toList());
    }

    /** Determine how many characters should be present on each rail.
     */
    private List<Integer> partitionLengths(String ciphertext) {
        // each journey down and up the rails consumes 2 * (numRails - 1) characters
        int cycleLength = 2 * (numRails - 1);
        int baseRailLength = ciphertext.length() / cycleLength;
        int leftovers = ciphertext.length() % cycleLength;

        // the inner rails consume twice as much as the top & bottom rails
        List<Integer> lengths = IntStream
                .rangeClosed(1, numRails)
                .map(i -> baseRailLength * ((i == 1 || i == numRails) ? 1 : 2))
                .boxed()
                .collect(Collectors.toList());

        // handle the leftovers: add 1 to each rail starting from the top
        RailWalker rw = new RailWalker(numRails);
        IntStream.rangeClosed(1, leftovers)
                .forEach(i -> {
                    int railIdx = rw.next();
                    lengths.set(railIdx, 1 + lengths.get(railIdx));
                });

        return lengths;
    }
}
