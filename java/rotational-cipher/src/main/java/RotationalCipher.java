class RotationalCipher {

    private static final String ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    private int shiftKey;

    RotationalCipher(int shiftKey) {
        this.shiftKey = shiftKey;
    }

    String rotate(String data) {
        StringBuilder sb = new StringBuilder();
        data.chars().forEach(ch -> sb.append(rotateChar((char) ch)));
        return sb.toString();
    }

    private char rotateChar(char ch) {
        Character uc = Character.toUpperCase(ch);
        int i = ALPHABET.indexOf(uc);
        if (i == -1) return ch;

        int shifted = (i + shiftKey) % ALPHABET.length();
        char coded = ALPHABET.charAt(shifted);
        return (ch == uc ? coded : Character.toLowerCase(coded));
    }
}
