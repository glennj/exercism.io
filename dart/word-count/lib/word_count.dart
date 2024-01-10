extension on String {
  String trimChar(String char) {
    var re = '(?:^${char}+)|(?:${char}+\$)';
    return this.replaceAll(RegExp(re), '');
  }

  String trimSingleQuotes() => trimChar("'");
}

Map<String, int> incrCount(Map<String, int> count, String word) =>
    count..update(word, (c) => c + 1, ifAbsent: () => 1);

class WordCount {
  Map<String, int> countWords(String input) => RegExp(r"[\w']+")
      .allMatches(input.toLowerCase())
      .map((match) => match[0]!.trimSingleQuotes())
      .where((word) => word.isNotEmpty)
      .fold({}, incrCount);
}
