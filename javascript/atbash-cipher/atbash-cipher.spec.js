import { encode } from './atbash-cipher';

describe('encode', () => {
  test('encodes no', () => expect(encode('no')).toEqual('ml'));

  test('encodes yes', () => expect(encode('yes')).toEqual('bvh'));

  test('encodes OMG', () => expect(encode('OMG')).toEqual('lnt'));

  test('encodes O M G', () => expect(encode('O M G')).toEqual('lnt'));

  test('encodes long words', () => expect(encode('mindblowingly')).toEqual('nrmwy oldrm tob'));

  test('encodes numbers', () => expect(encode('Testing, 1 2 3, testing.'))
    .toEqual('gvhgr mt123 gvhgr mt'));

  test('encodes sentences', () => expect(encode('Truth_is fiction.')).toEqual('gifgs rhurx grlm'));

  test('encodes all the things', () => expect(encode('The quick brown fox jumps over the lazy dog.'))
    .toEqual('gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt'));
});
