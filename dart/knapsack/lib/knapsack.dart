import 'dart:math';

typedef Item = ({int weight, int value});

class Knapsack {
  final int maxWeight;
  Knapsack({required this.maxWeight});

  // Using the algorithm at
  // https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem

  int maxValue(List<Item> items) {
    final m =
        List.generate(items.length + 1, (_) => List.filled(maxWeight + 1, 0));

    var i = 0;
    for (final item in items) {
      i += 1;
      for (var w = 0; w <= maxWeight; w++)
        m[i][w] = item.weight > w
            ? m[i - 1][w]
            : max(m[i - 1][w], item.value + m[i - 1][w - item.weight]);
    }
    return m[items.length][maxWeight];
  }
}
