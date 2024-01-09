import 'package:secret_handshake/secret_handshake.dart';
import 'package:test/test.dart';

void main() {
  final secretHandshake = SecretHandshake();

  group('SecretHandshake', () {
    test('wink for 1', () {
      final result = secretHandshake.commands(1);
      expect(result, equals(<String>['wink']));
    }, skip: false);

    test('double blink for 10', () {
      final result = secretHandshake.commands(2);
      expect(result, equals(<String>['double blink']));
    }, skip: false);

    test('close your eyes for 100', () {
      final result = secretHandshake.commands(4);
      expect(result, equals(<String>['close your eyes']));
    }, skip: false);

    test('jump for 1000', () {
      final result = secretHandshake.commands(8);
      expect(result, equals(<String>['jump']));
    }, skip: false);

    test('combine two actions', () {
      final result = secretHandshake.commands(3);
      expect(result, equals(<String>['wink', 'double blink']));
    }, skip: false);

    test('reverse two actions', () {
      final result = secretHandshake.commands(19);
      expect(result, equals(<String>['double blink', 'wink']));
    }, skip: false);

    test('reversing one action gives the same action', () {
      final result = secretHandshake.commands(24);
      expect(result, equals(<String>['jump']));
    }, skip: false);

    test('reversing no actions still gives no actions', () {
      final result = secretHandshake.commands(16);
      expect(result, equals(<String>[]));
    }, skip: false);

    test('all possible actions', () {
      final result = secretHandshake.commands(15);
      expect(result, equals(<String>['wink', 'double blink', 'close your eyes', 'jump']));
    }, skip: false);

    test('reverse all possible actions', () {
      final result = secretHandshake.commands(31);
      expect(result, equals(<String>['jump', 'close your eyes', 'double blink', 'wink']));
    }, skip: false);

    test('do nothing for zero', () {
      final result = secretHandshake.commands(0);
      expect(result, equals(<String>[]));
    }, skip: false);
  });
}
