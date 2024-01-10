class Triangle {
  bool equilateral(num a, num b, num c) => _test([a, b, c], [1]);
  bool isosceles(num a, num b, num c) => _test([a, b, c], [1, 2]);
  bool scalene(num a, num b, num c) => _test([a, b, c], [3]);

  bool _test(List<num> sides, List<int> expected) {
    sides.sort();
    return sides[0] > 0
        && sides[0] + sides[1] > sides[2]
        && expected.contains(sides.toSet().length);
  }
}
