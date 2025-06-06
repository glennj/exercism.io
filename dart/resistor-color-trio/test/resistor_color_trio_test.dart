import 'package:resistor_color_trio/resistor_color_trio.dart';
import 'package:test/test.dart';

void main() {
  final resistorColorTrio = ResistorColorTrio();

  group('ResistorColorTrio', () {
    test('Orange and orange and black', () {
      final result = resistorColorTrio.label(['orange', 'orange', 'black']);
      expect(result, equals("33 ohms"));
    }, skip: false);

    test('Blue and grey and brown', () {
      final result = resistorColorTrio.label(['blue', 'grey', 'brown']);
      expect(result, equals("680 ohms"));
    }, skip: false);

    test('Red and black and red', () {
      final result = resistorColorTrio.label(['red', 'black', 'red']);
      expect(result, equals("2 kiloohms"));
    }, skip: false);

    test('Green and brown and orange', () {
      final result = resistorColorTrio.label(['green', 'brown', 'orange']);
      expect(result, equals("51 kiloohms"));
    }, skip: false);

    test('Yellow and violet and yellow', () {
      final result = resistorColorTrio.label(['yellow', 'violet', 'yellow']);
      expect(result, equals("470 kiloohms"));
    }, skip: false);

    test('Blue and violet and blue', () {
      final result = resistorColorTrio.label(['blue', 'violet', 'blue']);
      expect(result, equals("67 megaohms"));
    }, skip: false);

    test('Minimum possible value', () {
      final result = resistorColorTrio.label(['black', 'black', 'black']);
      expect(result, equals("0 ohms"));
    }, skip: false);

    test('Maximum possible value', () {
      final result = resistorColorTrio.label(['white', 'white', 'white']);
      expect(result, equals("99 gigaohms"));
    }, skip: false);

    test('First two colors make an invalid octal number', () {
      final result = resistorColorTrio.label(['black', 'grey', 'black']);
      expect(result, equals("8 ohms"));
    }, skip: false);

    test('Ignore extra colors', () {
      final result = resistorColorTrio.label(['blue', 'green', 'yellow', 'orange']);
      expect(result, equals("650 kiloohms"));
    }, skip: false);
  });
}
