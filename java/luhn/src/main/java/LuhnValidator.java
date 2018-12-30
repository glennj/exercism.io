import java.util.List;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

class LuhnValidator {

    boolean isValid(String candidate) {
        candidate = candidate.replaceAll(" ", "");
        if (!candidate.matches("^\\d+$")) return false;
        if (candidate.length() < 2) return false;

        List<Character> digits = str2list(candidate);
        Collections.reverse(digits);
        int i = 0;
        int sum = 0;
        for (char digit : digits) {
            sum += luhnValue(digit, i);
            i++;
        }
        return sum % 10 == 0;
    }

    private int luhnValue(char digit, int i) {
        int number = Character.getNumericValue(digit);
        if (i % 2 == 1) {
            number *= 2;
            if (number >= 10) number -= 9;
        }
        return number;
    }

    private List<Character> str2list(String s) {
        return s.chars()
                .mapToObj(c -> (char) c)
                .collect(Collectors.toList());
    }
}
