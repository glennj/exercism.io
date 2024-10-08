import 'package:run_length_encoding/run_length_encoding.dart';
import 'package:test/test.dart';

void main() {
  final rle = RunLengthEncoder();

  group('Encoding', () {
    test('empty string', () {
      final encoded = rle.encode("");
      expect(encoded, equals(""));
    }, skip: false);

    test('single characters only are encoded without count', () {
      final encoded = rle.encode("XYZ");
      expect(encoded, equals("XYZ"));
    }, skip: false);

    test('string with no single characters', () {
      final encoded = rle.encode("AABBBCCCC");
      expect(encoded, equals("2A3B4C"));
    }, skip: false);

    test('single characters mixed with repeated characters', () {
      final encoded = rle.encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB");
      expect(encoded, equals("12WB12W3B24WB"));
    }, skip: false);

    test('multiple whitespace mixed in string', () {
      final encoded = rle.encode("  hsqq qww  ");
      expect(encoded, equals("2 hs2q q2w2 "));
    }, skip: false);

    test('lowercase characters', () {
      final encoded = rle.encode("aabbbcccc");
      expect(encoded, equals("2a3b4c"));
    }, skip: false);
  });

  group('Decoding', () {
    test('empty string', () {
      final decoded = rle.decode("");
      expect(decoded, equals(""));
    }, skip: false);

    test('single characters only', () {
      final decoded = rle.decode("XYZ");
      expect(decoded, equals("XYZ"));
    }, skip: false);

    test('string with no single characters', () {
      final decoded = rle.decode("2A3B4C");
      expect(decoded, equals("AABBBCCCC"));
    }, skip: false);

    test('single characters with repeated characters', () {
      final decoded = rle.decode("12WB12W3B24WB");
      expect(decoded, equals("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"));
    }, skip: false);

    test('multiple whitespace mixed in string', () {
      final decoded = rle.decode("2 hs2q q2w2 ");
      expect(decoded, equals("  hsqq qww  "));
    }, skip: false);

    test('lowercase string', () {
      final decoded = rle.decode("2a3b4c");
      expect(decoded, equals("aabbbcccc"));
    }, skip: false);
  });

  group('Encode then Decode', () {
    test('encode followed by decode gives original string', () {
      final input = "zzz ZZ  zZ";
      expect(rle.decode(rle.encode(input)), equals(input));
    }, skip: false);
  });
}
