import 'package:atbash_cipher/atbash_cipher.dart';
import 'package:test/test.dart';

void main() {
  final atbashCipher = AtbashCipher();

  group('AtbashCipher', () {
    group('encode', () {
      test('encode yes', () {
        final result = atbashCipher.encode("yes");
        expect(result, equals("bvh"));
      }, skip: false);

      test('encode no', () {
        final result = atbashCipher.encode("no");
        expect(result, equals("ml"));
      }, skip: false);

      test('encode OMG', () {
        final result = atbashCipher.encode("OMG");
        expect(result, equals("lnt"));
      }, skip: false);

      test('encode spaces', () {
        final result = atbashCipher.encode("O M G");
        expect(result, equals("lnt"));
      }, skip: false);

      test('encode mindblowingly', () {
        final result = atbashCipher.encode("mindblowingly");
        expect(result, equals("nrmwy oldrm tob"));
      }, skip: false);

      test('encode numbers', () {
        final result = atbashCipher.encode("Testing,1 2 3, testing.");
        expect(result, equals("gvhgr mt123 gvhgr mt"));
      }, skip: false);

      test('encode deep thought', () {
        final result = atbashCipher.encode("Truth is fiction.");
        expect(result, equals("gifgs rhurx grlm"));
      }, skip: false);

      test('encode all the letters', () {
        final result = atbashCipher.encode("The quick brown fox jumps over the lazy dog.");
        expect(result, equals("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"));
      }, skip: false);
    });

    group('decode', () {
      test('decode exercism', () {
        final result = atbashCipher.decode("vcvix rhn");
        expect(result, equals("exercism"));
      }, skip: false);

      test('decode a sentence', () {
        final result = atbashCipher.decode("zmlyh gzxov rhlug vmzhg vkkrm thglm v");
        expect(result, equals("anobstacleisoftenasteppingstone"));
      }, skip: false);

      test('decode numbers', () {
        final result = atbashCipher.decode("gvhgr mt123 gvhgr mt");
        expect(result, equals("testing123testing"));
      }, skip: false);

      test('decode all the letters', () {
        final result = atbashCipher.decode("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt");
        expect(result, equals("thequickbrownfoxjumpsoverthelazydog"));
      }, skip: false);

      test('decode with too many spaces', () {
        final result = atbashCipher.decode("vc vix    r hn");
        expect(result, equals("exercism"));
      }, skip: false);

      test('decode with no spaces', () {
        final result = atbashCipher.decode("zmlyhgzxovrhlugvmzhgvkkrmthglmv");
        expect(result, equals("anobstacleisoftenasteppingstone"));
      }, skip: false);
    });
  });
}
