import 'dart:math';

class Darts {
  int score(num x, num y) {
    var p = Point<num>(x, y);
    return DartsRing.values.firstWhere((ring) => ring.contains(p)).score;
  }
}

enum DartsRing {
  bullseye(radius: 1, score: 10),
  middle(radius: 5, score: 5),
  outer(radius: 10, score: 1),
  offBoard(radius: double.maxFinite, score: 0);

  const DartsRing({required this.radius, required this.score});

  final double radius;
  final int score;

  bool contains(Point<num> p) => Point<num>(0, 0).distanceTo(p) <= this.radius;
}
