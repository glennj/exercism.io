import java.util.Iterator;

class Hamming {

    private String left;
    private String right;

    Hamming(String leftStrand, String rightStrand) {
        if (leftStrand.length() != rightStrand.length())
            throw new IllegalArgumentException("strands must be of equal length");

        left = leftStrand;
        right = rightStrand;
    }

    int getHammingDistance() {
        // int distance = 0;
        // for (int i = 0; i < left.length(); i++) {
        //     if (left.codePointAt(i) != right.codePointAt(i))
        //         distance++;
        // }
        // return distance;

        Iterator<Integer> rChars = right.codePoints().iterator();
        return (int) left
            .codePoints()
            .filter(c -> c != rChars.next())
            .count();
    }
}
