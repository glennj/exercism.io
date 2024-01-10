import 'package:robot_simulator/orientation.dart';
import 'package:robot_simulator/position.dart';
import 'dart:math';

class Robot {
  Position position;
  Orientation orientation;
  final _orientations = Orientation.values;

  Robot(this.position, this.orientation);

  void move(String instructions) {
    for (var instruction in instructions.split('')) {
      switch (instruction) {
        case 'R' || 'L':
          _turn(instruction);
        case 'A':
          _advance();
        default:
          throw Exception('Invalid instruction: $instruction');
      }
    }
  }

  void _turn(String direction) {
    var addend = direction == 'R' ? 1 : -1;
    var nextIdx = (orientation.index + addend) % _orientations.length;
    orientation = _orientations[nextIdx];
  }

  void _advance() {
    var radians = pi / 2 * ((1 - orientation.index) % _orientations.length);
    position.x += cos(radians).round();
    position.y += sin(radians).round();
  }
}
