import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class CryptoSquare {
    private int size = 0;
    private final String ciphertext;

    CryptoSquare(String plaintext) {
        String normalized = prepare(plaintext);
        ciphertext = normalized.isEmpty()
                ? ""
                : encode(split(normalized));
    }

    String getCiphertext() { return ciphertext; }

    private String encode(List<String> rows) {
        return IntStream
                .range(0, size)
                // transpose rows to columns
                .mapToObj(i -> charStreamToString(
                        rows.stream().map(row -> row.charAt(i))
                ))
                // then join columns
                .collect(Collectors.joining(" "));
    }

    private List<String> split(String text) {
        return IntStream
                .range(0, text.length() / size)
                .mapToObj(i -> text.substring(i * size, (i + 1) * size))
                .collect(Collectors.toList());
    }

    private String prepare(String plaintext) {
        StringBuilder normalized = new StringBuilder(
                plaintext.replaceAll("\\W", "").toLowerCase()
        );
        this.size = (int)Math.ceil(Math.sqrt(normalized.length()));

        if (size > 0) {
            // pad with spaces so the length is a multiple of `size`
            normalized.append(String.join(
                    "",
                    Collections.nCopies(normalized.length() % size, " ")
            ));
        }

        return normalized.toString();
    }

    private String charStreamToString(Stream<Character> stream) {
        return stream
                .collect(
                        StringBuilder::new,
                        StringBuilder::append,
                        StringBuilder::append
                )
                .toString();
    }
}
