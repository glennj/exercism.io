import 'package:scrabble_score/scrabble_score.dart';
import 'package:test/test.dart';

void main() {
  group('ScrabbleScore', () {
    test('lowercase letter', () {
      final result = score('a');
      expect(result, equals(1));
    }, skip: false);

    test('uppercase letter', () {
      final result = score('A');
      expect(result, equals(1));
    }, skip: false);

    test('valuable letter', () {
      final result = score('f');
      expect(result, equals(4));
    }, skip: false);

    test('short word', () {
      final result = score('at');
      expect(result, equals(2));
    }, skip: false);

    test('short, valuable word', () {
      final result = score('zoo');
      expect(result, equals(12));
    }, skip: false);

    test('medium word', () {
      final result = score('street');
      expect(result, equals(6));
    }, skip: false);

    test('medium, valuable word', () {
      final result = score('quirky');
      expect(result, equals(22));
    }, skip: false);

    test('long, mixed-case word', () {
      final result = score('OxyphenButazone');
      expect(result, equals(41));
    }, skip: false);

    test('english-like word', () {
      final result = score('pinata');
      expect(result, equals(8));
    }, skip: false);

    test('empty input', () {
      final result = score('');
      expect(result, equals(0));
    }, skip: false);

    test('entire alphabet available', () {
      final result = score('abcdefghijklmnopqrstuvwxyz');
      expect(result, equals(87));
    }, skip: false);
  });
}
