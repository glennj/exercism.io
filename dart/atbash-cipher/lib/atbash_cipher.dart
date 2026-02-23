class AtbashCipher {
  String decode(String ciphertext) =>
      ciphertext.splitMapJoin('', onNonMatch: (char) => translation[char] ?? '');

  String encode(String plaintext) =>
      decode(plaintext.toLowerCase()).insertSpaces();
}

const translation = {
  'a': 'z', 'b': 'y', 'c': 'x', 'd': 'w', 'e': 'v', 'f': 'u', 'g': 't',
  'h': 's', 'i': 'r', 'j': 'q', 'k': 'p', 'l': 'o', 'm': 'n',
  'n': 'm', 'o': 'l', 'p': 'k', 'q': 'j', 'r': 'i', 's': 'h',
  't': 'g', 'u': 'f', 'v': 'e', 'w': 'd', 'x': 'c', 'y': 'b', 'z': 'a',
  '1': '1', '2': '2', '3': '3', '4': '4', '5': '5',
  '6': '6', '7': '7', '8': '8', '9': '9', '0': '0'
};

extension StringGrouping on String {
  String insertSpaces({int every = 5}) =>
      RegExp(".{1,${every}}").allMatches(this).map((m) => m.group(0)).join(' ');
}
