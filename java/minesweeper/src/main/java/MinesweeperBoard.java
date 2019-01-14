import java.util.ArrayList;
import java.util.List;

class MinesweeperBoard {
    private int[][] board;

    MinesweeperBoard(List<String> rows) {
        board = rows.stream()
                .map(row -> row
                        .chars()
                        .map(c -> (char)c == '*' ? -1 : 0)
                        .toArray()
                )
                .toArray(int[][]::new);

        for (int r = 0; r < board.length; r++) {
            for (int c = 0; c < board[r].length; c++) {
                if (board[r][c] == -1)
                    incrementNeighbours(r, c);
            }
        }
    }

    // the cell count has already been calculated.
    // this is basically a `toString` operation.
    List<String> withNumbers() {
        List<String> result = new ArrayList<>();
        for (int r = 0; r < board.length; r++) {
            StringBuilder sb = new StringBuilder();
            for (int c = 0; c < board[r].length; c++) {
                switch (board[r][c]) {
                    case -1: sb.append('*'); break;
                    case  0: sb.append(' '); break;
                    default: sb.append(board[r][c]);
                }
            }
            result.add(sb.toString());
        }
        return result;
    }

    private void incrementNeighbours(int r, int c) {
        for (int i = r-1; i <= r+1; i++) {
            if (i < 0 || i == board.length) continue;
            for (int j = c-1; j <= c+1; j++) {
                if (j < 0 || j == board[r].length) continue;
                if (i == r && j == c) continue;
                if (board[i][j] != -1) board[i][j]++;
            }
        }
    }
}
