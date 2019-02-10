import java.util.concurrent.atomic.AtomicInteger;

class LuhnValidator {

    boolean isValid(String candidate) {
        candidate = candidate.replaceAll(" ", "");
        if (candidate.matches(".*\\D.*")) return false;
        if (candidate.length() < 2) return false;

        AtomicInteger i = new AtomicInteger(candidate.length());
        int sum = candidate
                .chars()
                .map(c -> luhnValue((char) c, i.decrementAndGet()))
                .sum();

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
}
