import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class RectangleCounter {

    private static class Position implements Comparable<Position> {
        final int row, col;
        Position(int row, int col) {
            this.row = row;
            this.col = col;
        }
        boolean equals(Position other) {
            return this.row == other.row && this.col == other.col;
        }
        public int hashCode() {
            return Objects.hash(row, col);
        }
        public int compareTo(Position other) {
            int c = Integer.compare(this.row, other.row);
            if (c != 0) return c;
            return Integer.compare(this.col, other.col);
        }
    }

    private char[][] grid;
    private List<Position> vertices;

    int countRectangles(String[] input) {
        grid = new char[input.length][];
        for (int i = 0; i < input.length; i++) {
            grid[i] = input[i].toCharArray();
        }

        findVertices();

        int sum = 0;
        for (int row = 0; row < grid.length - 1; row++) {
            List<Position> topVertices = verticesInRow(row);
            while (topVertices.size() > 1) {
                Position topLeftVertex = topVertices.remove(0);
                for (Position topRightVertex : topVertices) {
                    sum += countRectangles(topLeftVertex, topRightVertex);
                }
            }
        }

        return sum;
    }

    private void findVertices() {
        vertices = new ArrayList<>();
        for (int row = 0; row < grid.length; row++) {
            for (int col = 0; col < grid[row].length; col++) {
                if (grid[row][col] == '+') {
                    vertices.add(new Position(row, col));
                }
            }
        }
    }

    private List<Position> verticesInRow(int row) {
        return vertices
                .stream()
                .filter(v -> v.row == row)
                .collect(Collectors.toList());
    }

    private int countRectangles(Position topLeft, Position topRight) {
        return (int) vertices
                .stream()
                .filter(v -> v.col == topLeft.col && v.row > topLeft.row)
                .filter(bottomLeft -> hasBottomRightVertex(bottomLeft, topRight))
                .filter(bottomLeft -> hasCompleteBorder(bottomLeft, topRight))
                .count();
    }

    private boolean hasBottomRightVertex(Position bottomLeft, Position topRight) {
        return 1 == vertices
                .stream()
                .filter(v -> v.row == bottomLeft.row && v.col == topRight.col)
                .count();
    }

    private static Pattern horizEdge = Pattern.compile("^[+][-+]*[+]$");
    private static Pattern vertEdge = Pattern.compile("^[+][|+]*[+]$");

    private boolean hasCompleteBorder(Position bottomLeft, Position topRight) {
        StringBuilder a = new StringBuilder();
        StringBuilder b = new StringBuilder();
        for (int col = bottomLeft.col; col <= topRight.col; col++) {
            a.append(grid[bottomLeft.row][col]);
            b.append(grid[topRight.row][col]);
        }
        if (!horizEdge.matcher(a.toString()).matches()) return false;
        if (!horizEdge.matcher(b.toString()).matches()) return false;

        a = new StringBuilder();
        b = new StringBuilder();
        for (int row = topRight.row; row <= bottomLeft.row; row++) {
            a.append(grid[row][bottomLeft.col]);
            b.append(grid[row][topRight.col]);
        }
        if (!vertEdge.matcher(a.toString()).matches()) return false;
        return vertEdge.matcher(b.toString()).matches();
    }
}
