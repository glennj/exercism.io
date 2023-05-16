public type Item record {
    int weight;
    int value;
};

// Using the algorithm at
// https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem
//
public function maximumValue(Item[] items, int maximumWeight) returns int {
    int[][] m = [];
    int i = 0;
    foreach int j in int:range(0, maximumWeight + 1, 1) {
        m[i][j] = 0;
    }
    foreach Item item in items {
        i += 1;
        foreach int j in int:range(0, maximumWeight + 1, 1) {
            m[i][j] = item.weight > j
                ? m[i - 1][j]
                : int:max(m[i - 1][j], m[i - 1][j - item.weight] + item.value);
        }
    }
    return m[items.length()][maximumWeight];
}
