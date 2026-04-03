typedef InType = Map<String, List<String>>;
typedef OutType = Map<String, int>;

class Etl {
  // using lots of lazy Iterables, so it should be pretty quick.
  OutType transform(InType original) =>
    Map.fromEntries(
      original.entries
        .map((entry) => MapEntry(int.parse(entry.key), entry.value))
        .expand((entry) => entry.value.expand((letter) => [MapEntry(letter.toLowerCase(), entry.key)]))
    );
}
