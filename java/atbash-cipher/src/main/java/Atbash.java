import java.util.function.IntUnaryOperator;

public class Atbash {
    public String encode(String input) {
        return groupBy5(__code(input));
    }

    public String decode(String input) {
        return __code(input);
    }

    // convert 'a' to 'z', 'b' to 'y', etc
    private static IntUnaryOperator encoder = c ->
            Character.isLetter(c)
                    ? 'a' + (26 - (c - ('a' - 1)))
                    : c;

    private String __code(String input) {
        return input
                .codePoints()
                .filter(Character::isLetterOrDigit)
                .map(Character::toLowerCase)
                .map(encoder)
                .collect(
                        StringBuilder::new,
                        StringBuilder::appendCodePoint,
                        StringBuilder::append
                )
                .toString();
    }

    private String groupBy5(String input) {
        return input
                .replaceAll(".{1,5}", "$0 ")
                .replaceFirst(" $", "");
    }
}
