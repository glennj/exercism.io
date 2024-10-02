class AtbashCipher {
  String decode(String ciphertext) =>
    ciphertext.splitMapJoin('', onNonMatch: _translate);

  String encode(String plaintext) =>
    decode(plaintext.toLowerCase()).insertSpacesEvery(5);

  String _translate(String character) => switch (character) {
      'a' => 'z', 'b' => 'y', 'c' => 'x', 'd' => 'w', 'e' => 'v', 'f' => 'u', 'g' => 't',
      'h' => 's', 'i' => 'r', 'j' => 'q', 'k' => 'p', 'l' => 'o', 'm' => 'n',
      'n' => 'm', 'o' => 'l', 'p' => 'k', 'q' => 'j', 'r' => 'i', 's' => 'h',
      't' => 'g', 'u' => 'f', 'v' => 'e', 'w' => 'd', 'x' => 'c', 'y' => 'b', 'z' => 'a',
      '1' => '1', '2' => '2', '3' => '3', '4' => '4', '5' => '5',
      '6' => '6', '7' => '7', '8' => '8', '9' => '9', '0' => '0',
      _ => ''
    };
}

extension StringGrouping on String {
  String insertSpacesEvery(int count) =>
    RegExp(".{1,${count}}").allMatches(this).map((m) => m.group(0)).join(' ');
}