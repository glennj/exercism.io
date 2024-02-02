import 'package:matching_brackets/matching_brackets.dart';
import 'package:test/test.dart';

void main() {
  final matchingBrackets = MatchingBrackets();

  group('MatchingBrackets', () {
    test('paired square brackets', () {
      final result = matchingBrackets.isPaired('[]');
      expect(result, equals(true));
    }, skip: false);

    test('empty string', () {
      final result = matchingBrackets.isPaired('');
      expect(result, equals(true));
    }, skip: false);

    test('unpaired brackets', () {
      final result = matchingBrackets.isPaired('[[');
      expect(result, equals(false));
    }, skip: false);

    test('wrong ordered brackets', () {
      final result = matchingBrackets.isPaired('}{');
      expect(result, equals(false));
    }, skip: false);

    test('wrong closing bracket', () {
      final result = matchingBrackets.isPaired('{]');
      expect(result, equals(false));
    }, skip: false);

    test('paired with whitespace', () {
      final result = matchingBrackets.isPaired('{ }');
      expect(result, equals(true));
    }, skip: false);

    test('partially paired brackets', () {
      final result = matchingBrackets.isPaired('{[])');
      expect(result, equals(false));
    }, skip: false);

    test('simple nested brackets', () {
      final result = matchingBrackets.isPaired('{[]}');
      expect(result, equals(true));
    }, skip: false);

    test('several paired brackets', () {
      final result = matchingBrackets.isPaired('{}[]');
      expect(result, equals(true));
    }, skip: false);

    test('paired and nested brackets', () {
      final result = matchingBrackets.isPaired('([{}({}[])])');
      expect(result, equals(true));
    }, skip: false);

    test('unopened closing brackets', () {
      final result = matchingBrackets.isPaired('{[)][]}');
      expect(result, equals(false));
    }, skip: false);

    test('unpaired and nested brackets', () {
      final result = matchingBrackets.isPaired('([{])');
      expect(result, equals(false));
    }, skip: false);

    test('paired and wrong nested brackets', () {
      final result = matchingBrackets.isPaired('[({]})');
      expect(result, equals(false));
    }, skip: false);

    test('paired and wrong nested brackets but innermost are correct', () {
      final result = matchingBrackets.isPaired('[({}])');
      expect(result, equals(false));
    }, skip: false);

    test('paired and incomplete brackets', () {
      final result = matchingBrackets.isPaired('{}[');
      expect(result, equals(false));
    }, skip: false);

    test('too many closing brackets', () {
      final result = matchingBrackets.isPaired('[]]');
      expect(result, equals(false));
    }, skip: false);

    test('early unexpected brackets', () {
      final result = matchingBrackets.isPaired(')()');
      expect(result, equals(false));
    }, skip: false);

    test('early mismatched brackets', () {
      final result = matchingBrackets.isPaired('{)()');
      expect(result, equals(false));
    }, skip: false);

    test('math expression', () {
      final result = matchingBrackets.isPaired('(((185 + 223.85) * 15) - 543)/2');
      expect(result, equals(true));
    }, skip: false);

    test('complex latex expression', () {
      final result = matchingBrackets
          .isPaired('\left(\begin{array}{cc} \frac{1}{3} & x\ \mathrm{e}^{x} &... x^2 \end{array}\right)');
      expect(result, equals(true));
    }, skip: false);
  });
}
