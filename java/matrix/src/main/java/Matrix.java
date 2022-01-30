import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Matrix {
    private List<List<Integer>> rows;
    private List<List<Integer>> cols;

    Matrix(String matrixAsString) {
        rows = splitText(matrixAsString);
        cols = transpose(rows);
    }

    int[] getRow(int rowNumber) {
        return list2array(rows.get(rowNumber - 1));
    }

    int[] getColumn(int columnNumber) {
        return list2array(cols.get(columnNumber - 1));
    }

    private List<List<Integer>> splitText(String matrixAsString) {
        return Arrays
                .stream(matrixAsString.split("\n"))
                .map(line ->
                        Arrays
                            .stream(line.split(" +"))
                            .map(Integer::parseInt)
                            .collect(Collectors.toList())
                )
                .collect(Collectors.toList());
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

    private int[] list2array(List<Integer> list) {
        return list
                .stream()
                .mapToInt(Integer::intValue)
                .toArray();
    }
}
