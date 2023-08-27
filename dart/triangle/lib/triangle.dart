class Triangle {
  bool equilateral(num a, num b, num c) => isTriangle(a, b, c) && numUniqSides(a, b, c) == 1;
  bool isosceles  (num a, num b, num c) => isTriangle(a, b, c) && numUniqSides(a, b, c) <= 2;
  bool scalene    (num a, num b, num c) => isTriangle(a, b, c) && numUniqSides(a, b, c) == 3;

  bool isTriangle(num a, num b, num c) {
    var sides = [a, b, c];
    sides.sort();
    return sides[0] > 0 && sides[0] + sides[1] > sides[2];
  }

  int numUniqSides(num a, num b, num c) => {a, b, c}.length;
}
