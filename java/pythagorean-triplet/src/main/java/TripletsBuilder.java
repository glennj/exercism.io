import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

class TripletsBuilder {
    private int maxLen = 0;
    private int perimeter = 0;

    TripletsBuilder withFactorsLessThanOrEqualTo(int len) {
        maxLen = len;
        return this;
    }

    TripletsBuilder thatSumTo(int perimeter) {
        this.perimeter = perimeter;
        return this;
    }

    List<PythagoreanTriplet> build() {
        List<PythagoreanTriplet> triplets = new ArrayList<>();

        int a = 0;
        while (true) {
            a++;
            int numerator = perimeter * (perimeter - 2 * a);
            int denominator = 2 * (perimeter - a);
            int b = (int) numerator / denominator;
            if (b < a)
                break;
            if (numerator % denominator != 0)
                continue;
            int c = perimeter - a - b;
            if (maxLen > 0 && c > maxLen)
                continue;
            triplets.add(new PythagoreanTriplet(a, b, c));
        }

        Collections.sort(triplets);
        return triplets;
    }
}
