import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class PythagoreanTriplet implements Comparable<PythagoreanTriplet> {
    private final int a, b, c;

    PythagoreanTriplet(int a, int b, int c) {
        int[] edges = new int[]{a, b, c};
        Arrays.sort(edges);
        if (edges[0] <= 0 || edges[0] + edges[1] < edges[2])
            throw new IllegalArgumentException("Not a triangle");
        if (edges[0] * edges[0] + edges[1] * edges[1] != edges[2] * edges[2])
            throw new IllegalArgumentException("Not a Pythagorean triangle");

        this.a = edges[0];
        this.b = edges[1];
        this.c = edges[2];
    }

    static TripletsBuilder makeTripletsList() {
        return new TripletsBuilder();
    }

    // required to determine if `List<PythagoreanTriplet> a` equals `List<PythagoreanTriplet> b`
    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        if (obj == this) return true;
        if (obj instanceof PythagoreanTriplet) {
            PythagoreanTriplet other = (PythagoreanTriplet) obj;
            return a == other.a && b == other.b && c == other.c;
        }
        return false;
    }

    // override hashCode if override equals
    @Override
    public int hashCode() {
        return Objects.hash(a, b, c);
    }

    // for sorting
    @Override
    public int compareTo(PythagoreanTriplet other) {
        int cmp = Integer.compare(a, other.a);
        if (cmp != 0) return cmp;
        cmp = Integer.compare(b, other.b);
        if (cmp != 0) return cmp;
        return Integer.compare(c, other.c);
    }
}

class TripletsBuilder {
    private int maxFactor = 0;
    private int perimeter = 0;

    TripletsBuilder withFactorsLessThanOrEqualTo(int len) {
        maxFactor = len;
        return this;
    }

    TripletsBuilder thatSumTo(int perimeter) {
        this.perimeter = perimeter;
        return this;
    }

    List<PythagoreanTriplet> build() {
        double r2 = Math.sqrt(2);
        // given the perimeter, this is the smallest possible hypotenuse.
        int cMin = (int)Math.floor(perimeter * r2 / (2 + r2));
        // and side b would have this min value such that a < b
        int bMin = (int)Math.floor(perimeter * r2 / (2 + 2 * r2));

        boolean wantStreamed = false;
        if (wantStreamed) {
            // `tripletsWhoseSumIs30000` test completes in ~6 sec
            return generateTripletsStreamed(cMin, bMin);
        }
        else {
            // `tripletsWhoseSumIs30000` test completes in ~1.5 sec
            return generateTripletsIterative(cMin, bMin);
        }
    }

    private List<PythagoreanTriplet> generateTripletsStreamed(int cMin, int bMin) {
        return IntStream
                .rangeClosed(cMin, maxFactor)
                .boxed()
                .flatMap(c -> IntStream
                        .range(bMin, c)
                        .mapToObj(b -> {
                            double edge = (int)Math.sqrt(c * c - b * b);
                            int a = (int) edge;
                            // if edge != a, then not a Pythagorean triangle
                            return new int[]{a, b};
                        })
                        .filter(tuple -> {
                            int a = tuple[0];
                            int b = tuple[1];
                            return a*a + b*b == c*c
                                    && a < b
                                    && a + b + c == perimeter;
                        })
                        .map(tuple ->
                                new PythagoreanTriplet(tuple[0], tuple[1], c)
                        )
                )
                .sorted()
                .collect(Collectors.toList());
    }

    private List<PythagoreanTriplet> generateTripletsIterative(int cMin, int bMin) {
        List<PythagoreanTriplet> triplets = new ArrayList<>();
        for (int c = maxFactor; c >= cMin; c--) {
            for (int b = c - 1; b >= bMin; b--) {
                double edge = Math.sqrt(c * c - b * b);
                int a = (int) edge;
                // cannot be a pythagorean triangle if `edge` is not an Integral value.
                if (edge == a && a < b && a + b + c == perimeter) {
                    triplets.add(new PythagoreanTriplet(a, b, c));
                }
            }
        }
        Collections.sort(triplets);
        return triplets;
    }
}
