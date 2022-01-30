public class PhoneNumber {
    private String number;

    public PhoneNumber(String phoneNumber) throws IllegalArgumentException {
        // remove valid non-digits
        number = phoneNumber
            .replaceAll("[\\s().-]", "")
            .replaceFirst("^[+]", "");

        if (number.matches(".*\\p{Alpha}.*"))
            throw new IllegalArgumentException("letters not permitted");
        if (number.matches(".*\\p{Punct}.*"))
            throw new IllegalArgumentException("punctuations not permitted");

        if (number.length() <= 9)
            throw new IllegalArgumentException("incorrect number of digits");
        if (number.length() >= 12)
            throw new IllegalArgumentException("more than 11 digits");

        if (number.length() == 11)
            if (!number.startsWith("1"))
                throw new IllegalArgumentException("11 digits must start with 1");
            else
                number = number.substring(1);

        if (number.charAt(0) == '0')
            throw new IllegalArgumentException("area code cannot start with zero");
        if (number.charAt(0) == '1')
            throw new IllegalArgumentException("area code cannot start with one");
        if (number.charAt(3) == '0')
            throw new IllegalArgumentException("exchange code cannot start with zero");
        if (number.charAt(3) == '1')
            throw new IllegalArgumentException("exchange code cannot start with one");
    }

    public String getNumber() {
        return number;
    }
}
