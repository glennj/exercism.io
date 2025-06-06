import 'package:resistor_color_trio/resistor_color.dart';
import 'package:resistor_color_trio/resistor_color_duo.dart';
import 'dart:math';

class ResistorColorTrio {
  String label(List<String> colors) {
    var value = ResistorColorDuo().value(colors);
    var exponent = ResistorColor().colorCode(colors[2]);
    var resistance = value * pow(10, exponent).toInt();
    var idx = 0;

    while (resistance > 0 && resistance % 1000 == 0) {
      idx += 1;
      resistance ~/= 1000;
    }

    var prefix = ["", "kilo", "mega", "giga"][idx];

    return "${resistance} ${prefix}ohms";
  }
}
