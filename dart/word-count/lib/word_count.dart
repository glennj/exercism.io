extension on String {
  String trimChar(String char) {
    var re = '(?:^${char}+)|(?:${char}+\$)';
    return this.replaceAll(RegExp(re), '');
  }
}

class WordCount {
  Map<String, int> countWords(String input) =>
      RegExp(r"[\w']+")
        .allMatches(input.toLowerCase())
        .map((match) => match[0]!.trimChar("'"))
        .where((word) => word.isNotEmpty)
        .fold({}, (count, word) {
          count[word] = 1 + (count[word] ?? 0);
          return count;
        });
}
