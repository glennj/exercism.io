import java.util.Optional;

class ReverseString {
    String reverse(String inputString) {
        StringBuilder sb = new StringBuilder();
        char[] chars = Optional.ofNullable(inputString).orElse("").toCharArray();
        for (char c : chars) {
            sb.insert(0, c);
        }
        return sb.toString();
    }
}
