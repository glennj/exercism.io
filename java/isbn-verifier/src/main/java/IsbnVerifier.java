import java.util.stream.IntStream;

class IsbnVerifier {

    boolean isValid(String stringToVerify) {
        String isbn = stringToVerify.replaceAll("[^X\\d]", "");
        if (!isbn.matches("^\\d{9}[\\dX]$"))
            return false;
        
        int sum = IntStream
            .range(0, 10)
            .map(i -> (10 - i) * char2int(isbn.charAt(i)))
            .sum();
        
        return (sum % 11) == 0;
    }

    private int char2int(char digit) {
        return digit == 'X' ? 10 : Character.getNumericValue(digit);
    }
}
