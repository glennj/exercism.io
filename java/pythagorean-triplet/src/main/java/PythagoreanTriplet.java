import java.util.Objects;

class PythagoreanTriplet implements Comparable<PythagoreanTriplet> {
    private final int a, b, c;

    PythagoreanTriplet(int a, int b, int c) {
        int[] edges = new int[]{a, b, c};

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
