public class PhoneNumber {
    private String number;

    public PhoneNumber(String phoneNumber) throws IllegalArgumentException {
        if (!phoneNumber.matches("^[+]?[\\d\\h().-]+"))
            throw new IllegalArgumentException("Illegal character in phone number. Only digits, spaces, parentheses, hyphens or dots accepted.");

        number = phoneNumber.replaceAll("\\D", "");

        if (number.length() < 10 || number.length() > 11)
            throw new IllegalArgumentException("Number must be 10 or 11 digits");

        if (number.length() == 11)
            if (number.matches("^1.*"))
                number = number.substring(1);
            else
                throw new IllegalArgumentException("Can only have 11 digits if number starts with '1'");

        if (!number.matches("^[^01]..[^01].*"))
            throw new IllegalArgumentException("Illegal Area Or Exchange Code. Only 2-9 are valid digits");
    }

    public String getNumber() {
        return number;
    }
}
