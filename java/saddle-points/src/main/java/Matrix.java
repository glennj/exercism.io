import java.util.List;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Matrix {

    private final List<List<Integer>> rows;
    private final List<List<Integer>> cols;
    private final Set<MatrixCoordinate> saddlePoints;

    Matrix(List<List<Integer>> values) {
        rows = values;

        if (rows.size() == 0) {
            cols = new ArrayList<>();
            saddlePoints = new HashSet<>();
        }
        else {
            cols = transpose(rows);
            saddlePoints = findSaddlePoints();
        }
    }

    Set<MatrixCoordinate> getSaddlePoints() {
        return saddlePoints;
    }

    private List<List<Integer>> transpose(List<List<Integer>> rows) {
        return IntStream
                .range(0, rows.get(0).size())
                .mapToObj(col ->
                        IntStream
                                .range(0, rows.size())
                                .mapToObj(row -> rows.get(row).get(col))
                                .collect(Collectors.toList())
                )
                .collect(Collectors.toList());
    }

    private Set<MatrixCoordinate> findSaddlePoints() {
        Set<MatrixCoordinate> points = new HashSet<>();
        List<Integer> maxByRow = rows.stream().map(Collections::max).collect(Collectors.toList());
        List<Integer> minByCol = cols.stream().map(Collections::min).collect(Collectors.toList());

        for (int row = 0; row < maxByRow.size(); row++) {
            for (int col = 0; col < minByCol.size(); col++) {
                int value = rows.get(row).get(col);
                if (value == maxByRow.get(row) && value == minByCol.get(col)) {
                    points.add(new MatrixCoordinate(row+1, col+1));
                }
            }
        }
        return points;
    }
}
