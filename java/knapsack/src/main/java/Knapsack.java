import java.util.Arrays;
import java.util.List;

public class Knapsack {

    /*
    Algorithm taken from Wikipedia:
    https://en.wikipedia.org/wiki/Knapsack_problem

    Define m[i][j] to be the maximum value that can be attained
    with weight less than or equal to j using items up to i
    (first i items).

    We can define m[i][j] recursively as follows:

        m[0][j] = 0
        m[i][j] = m[i−1][j] if w[i] > j (the new item is more than the current weight limit)
        m[i][j] = max (m[i−1][j], m[i−1][j − w[i]] + v[i]) if w[i] ⩽ j
    */

    public int maximumValue(int maxWeight, List<Item> items) {

        // Values (stored in array v)
        // Weights (stored in array w)
        // Number of distinct items (n)
        // Knapsack capacity (W)
        // NOTE: The array "v" and array "w" are assumed to store all
        //       relevant values starting at index 1.

        int W = maxWeight;
        int n = items.size();
        int[][] m = new int[n + 1][W + 1];
        int[] w = new int[n + 1];
        int[] v = new int[n + 1];

        int i = 0;
        for (Item item : items) {
            i++;
            w[i] = item.getWeight();
            v[i] = item.getValue();
        }

        Arrays.fill(m[0], 0);

        for (i = 1; i <= n; i++)
            for (int j = 1; j <= W; j++)
                if (w[i] > j)
                    m[i][j] = m[i - 1][j];
                else
                    m[i][j] = Math.max(m[i - 1][j], m[i - 1][j - w[i]] + v[i]);

        return m[n][W];
    }

}