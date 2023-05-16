# Returns the saddle points in the given matrix.
#
# A saddle point is an value that is:
# - equal to the maximum value in its row, and
# - equal to the minimum value in its column.
#
# + matrix - an array of int arrays (i.e. two-dimensional matrix)
# + return - an array of maps [{"row": x, "column": y}, ...]
public function saddlePoints(int[][] matrix) returns map<int>[] {
    int[] rowMaxima = matrix.map(row => int:max(int:MIN_VALUE, ...row));

    int[] colMinima = [];
    foreach int j in 0 ..< matrix[0].length() {
        int[] column = matrix.map(row => row[j]);
        colMinima.push(int:min(int:MAX_VALUE, ...column));
    }

    map<int>[] saddlePoints = [];
    foreach int i in 0 ..< matrix.length() {
        foreach int j in 0 ..< matrix[0].length() {
            if matrix[i][j] == rowMaxima[i] && matrix[i][j] == colMinima[j] {
                saddlePoints.push({"row": i+1, "column": j+1});
            }
        }
    }
    return saddlePoints;
}
