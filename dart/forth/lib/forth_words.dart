class ForthWords {
  final _words = <String, List<String>>{};

  void add(List<String> tokens) {
    var word = tokens.removeAt(0);
    var number = int.tryParse(word);
    if (number != null) throw Exception('Invalid definition');

    tokens.removeLast(); // the trailing semicolon
    _words[word] = tokens.fold([], (defn, token) {
      return defn + (_words[token] ?? [token]);
    });
  }

  bool hasDefinition(String word) => _words.containsKey(word);

  List<String> get(String word) {
    var definition = _words[word];
    if (definition == null) throw Exception('Unknown command');
    return definition;
  }
}
