import java.util.ArrayList;
import java.util.List;

class VariableLengthQuantity {

    private static final int MSB  = 0b10000000; // 1 << 7
    private static final int MASK = 0b01111111; // MSB ^ 0xff
    private static final int SHIFT_AMT = 7;

    private String asHexString(long n) {
        return String.format("0x%x", n);
    }

    List<String> encode(List<Long> numbers) {
        List<String> result = new ArrayList<>();
        for (long n : numbers) {
            List<String> bytes = new ArrayList<>();
            int msb = 0;
            do {
                bytes.add(0, asHexString((n & MASK) | msb));
                msb = MSB;
                n >>= SHIFT_AMT;
            } while (n > 0);
            result.addAll(bytes);
        }
        return result;
    }

    List<String> decode(List<Long> bytes) throws IllegalArgumentException {
        if ((bytes.get(bytes.size() - 1) & MSB) != 0)
            throw new IllegalArgumentException("Invalid variable-length quantity encoding");

        List<String> result = new ArrayList<>();
        long n = 0;
        for (long b : bytes) {
            n = (n << SHIFT_AMT) + (b & MASK);
            if ((b & MSB) == 0) {
                result.add(asHexString(n));
                n = 0;
            }
        }
        return result;
    }
}

