import java.util.List;
import java.util.Arrays;
import static java.util.Comparator.naturalOrder;

class Triangle {

    private double a;
    private double b;
    private double c;

    Triangle(double side1, double side2, double side3) throws TriangleException {
        List<Double> sides = Arrays.asList(side1, side2, side3);
        sides.sort(naturalOrder());
        a = sides.get(0);
        b = sides.get(1);
        c = sides.get(2);

        if (a <= 0 || a + b < c)
            throw new TriangleException();
    }

    boolean isEquilateral() {
        return (a == b && b == c);
    }

    boolean isIsosceles() {
        return (a == b || a == c || b == c);
    }

    boolean isScalene() {
        return !isIsosceles();
    }
}
