import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SpiralMatrixBuilder {

    private int[][] matrix;
    private int x;
    private int y;

    public int[][] buildMatrixOfSize(int size) {
        initialize(size);

        for (int i = 0; i < size * size; i++) {
            matrix[y][x] = i + 1;
            nextCell();
        }

        return matrix;
    }

    private int size;
    private int step;

    private void initialize(int size) {
        matrix = new int[size][size];
        for (int i = 0; i < size; i++) {
            Arrays.fill(matrix[i], -1);
        }
        this.size = size;
        x = 0;
        y = 0;
        step = 0;
    }

    private int dx;
    private int dy;

    private void nextCell() {
        setDelta();

        if (x + dx == size || x + dx == -1 ||
            y + dy == size || y + dy == -1 ||
            matrix[y + dy][x + dx] != -1
        ) {
            step = (step + 1) % steps.size();
            setDelta();
        }

        x += dx;
        y += dy;
    }

    private static List<List<Integer>> steps = new ArrayList<>();
    static {
        steps.add(Arrays.asList( 1,  0));
        steps.add(Arrays.asList( 0,  1));
        steps.add(Arrays.asList(-1,  0));
        steps.add(Arrays.asList( 0, -1));
    }

    private void setDelta() {
        List<Integer> delta = steps.get(step);
        dx = delta.get(0);
        dy = delta.get(1);
    }
}
