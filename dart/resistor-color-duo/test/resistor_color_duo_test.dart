import 'package:resistor_color_duo/resistor_color_duo.dart';
import 'package:test/test.dart';

void main() {
  final resistorColorDuo = ResistorColorDuo();

  group('ResistorColorDuo', () {
    test('Brown and black', () {
      final result = resistorColorDuo.value(<String>['brown', 'black']);
      expect(result, equals(10));
    }, skip: false);

    test('Blue and grey', () {
      final result = resistorColorDuo.value(<String>['blue', 'grey']);
      expect(result, equals(68));
    }, skip: false);

    test('Yellow and violet', () {
      final result = resistorColorDuo.value(<String>['yellow', 'violet']);
      expect(result, equals(47));
    }, skip: false);

    test('White and red', () {
      final result = resistorColorDuo.value(<String>['white', 'red']);
      expect(result, equals(92));
    }, skip: false);

    test('Orange and orange', () {
      final result = resistorColorDuo.value(<String>['orange', 'orange']);
      expect(result, equals(33));
    }, skip: false);

    test('Ignore additional colors', () {
      final result = resistorColorDuo.value(<String>['green', 'brown', 'orange']);
      expect(result, equals(51));
    }, skip: false);

    test('Black and brown, one-digit', () {
      final result = resistorColorDuo.value(<String>['black', 'brown']);
      expect(result, equals(1));
    }, skip: false);
  });
}
