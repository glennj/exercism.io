public class BaseConverter {
    private int decimalValue = 0;

    BaseConverter(int fromBase, int[] digits) {
        if (fromBase <= 1)
            throw new IllegalArgumentException("Bases must be at least 2.");

        for (int i = 0; i < digits.length; i++) {
            if (digits[i] < 0)
                throw new IllegalArgumentException("Digits may not be negative.");
            if (digits[i] >= fromBase)
                throw new IllegalArgumentException("All digits must be strictly less than the base.");

            decimalValue = decimalValue * fromBase + digits[i];
        }
    }

    int[] convertToBase(int toBase) {
        if (toBase <= 1)
            throw new IllegalArgumentException("Bases must be at least 2.");

        if (decimalValue == 0) return new int[]{0};

        int toDigitsLength = 1 + (int)Math.floor(Math.log10(decimalValue) / Math.log10(toBase));
        int[] result = new int[toDigitsLength];

        for (int i = toDigitsLength - 1; decimalValue > 0; i--) {
            result[i] = decimalValue % toBase;
            decimalValue /= toBase;
        }
        return result;
    }
}
