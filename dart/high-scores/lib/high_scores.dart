import 'dart:math' as math;

extension MaxFromIntList on List<int> {
  int max() => reduce(math.max);
}

class HighScores {
  List<int> scores;

  HighScores(this.scores);

  int latest() => scores.last;
  int personalBest() => scores.max();

  List<int> personalTopThree() {
    // cascade notation
    List<int> sorted = List.from(scores)..sort((a, b) => b - a);
    return sorted.take(3).toList();
  }
}
