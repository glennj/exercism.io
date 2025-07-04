import 'package:isbn_verifier/isbn_verifier.dart';
import 'package:test/test.dart';

void main() {
  group('IsbnVerifier', () {
    test('valid isbn', () {
      final result = isValid('3-598-21508-8');
      expect(result, equals(true));
    }, skip: false);

    test('invalid isbn check digit', () {
      final result = isValid('3-598-21508-9');
      expect(result, equals(false));
    }, skip: true);

    test('valid isbn with a check digit of 10', () {
      final result = isValid('3-598-21507-X');
      expect(result, equals(true));
    }, skip: true);

    test('check digit is a character other than X', () {
      final result = isValid('3-598-21507-A');
      expect(result, equals(false));
    }, skip: true);

    test('invalid check digit in isbn is not treated as zero', () {
      final result = isValid('4-598-21507-B');
      expect(result, equals(false));
    }, skip: true);

    test('invalid character in isbn is not treated as zero', () {
      final result = isValid('3-598-P1581-X');
      expect(result, equals(false));
    }, skip: true);

    test('X is only valid as a check digit', () {
      final result = isValid('3-598-2X507-9');
      expect(result, equals(false));
    }, skip: true);

    test('X is only valid as a check digit', () {
      final result = isValid('3-598-2X507-6');
      expect(result, equals(false));
    }, skip: true);

    test('valid isbn without separating dashes', () {
      final result = isValid('3598215088');
      expect(result, equals(true));
    }, skip: true);

    test('isbn without separating dashes and X as check digit', () {
      final result = isValid('359821507X');
      expect(result, equals(true));
    }, skip: true);

    test('isbn without check digit and dashes', () {
      final result = isValid('359821507');
      expect(result, equals(false));
    }, skip: true);

    test('too long isbn and no dashes', () {
      final result = isValid('3598215078X');
      expect(result, equals(false));
    }, skip: true);

    test('too short isbn', () {
      final result = isValid('00');
      expect(result, equals(false));
    }, skip: true);

    test('isbn without check digit', () {
      final result = isValid('3-598-21507');
      expect(result, equals(false));
    }, skip: true);

    test('check digit of X should not be used for 0', () {
      final result = isValid('3-598-21515-X');
      expect(result, equals(false));
    }, skip: true);

    test('empty isbn', () {
      final result = isValid('');
      expect(result, equals(false));
    }, skip: true);

    test('input is 9 characters', () {
      final result = isValid('134456729');
      expect(result, equals(false));
    }, skip: true);

    test('invalid characters are not ignored after checking length', () {
      final result = isValid('3132P34035');
      expect(result, equals(false));
    }, skip: true);

    test('invalid characters are not ignored before checking length', () {
      final result = isValid('3598P215088');
      expect(result, equals(false));
    }, skip: true);

    test('input is too long but contains a valid isbn', () {
      final result = isValid('98245726788');
      expect(result, equals(false));
    }, skip: true);
  });
}
