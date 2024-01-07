const MIN_INT = 1 << 63;
int maxOf(List<int> ns) => ns.fold(MIN_INT, (max, n) => max >= n ? max : n);

class HighScores {
  List<int> scores;

  HighScores(this.scores);

  int latest() => this.scores.last;
  int personalBest() => maxOf(this.scores);

  List<int> personalTopThree() {
    List<int> copy = List.from(this.scores);
    copy.sort((a, b) => b - a);
    return copy.take(3).toList();
  }
}
