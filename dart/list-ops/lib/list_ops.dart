/* I use the following builtin List functionality
 * - forEach method
 * - add method
 * - insert method
 *
 * This won't be the most efficient solution: I prioritized code reuse.
 *
 * I'm not 100% sold on the formatting, but this is what `dart format` gives.
 */

extension ListOps on List {
  // foldl is the rock upon which ...
  U foldl<T, U>(U initial, U Function(U acc, T elem) fn) {
    var result = initial;
    forEach((elem) => result = fn(result, elem));
    return result;
  }

  int count() => foldl(0, (int len, _) => len + 1);

  void append<T>(List<T> other) => other.forEach(add);

  List<T> concat<T>() => foldl([], (catted, List<T> sublist) {
        catted.append(sublist);
        return catted;
      });

  List<T> filter<T>(bool Function(T elem) predicate) =>
      foldl([], (filtered, T elem) {
        if (predicate(elem)) {
          filtered.add(elem);
        }
        return filtered;
      });

  List<T> myMap<T>(T Function(T elem) fn) => foldl([], (mapped, T elem) {
        mapped.add(fn(elem));
        return mapped;
      });

  List<T> reverse<T>() => foldl([], (reversed, T elem) {
        reversed.insert(0, elem);
        return reversed;
      });

  U foldr<T, U>(U initial, U Function(T elem, U acc) fn) =>
      reverse().foldl(initial, (U acc, T elem) => fn(elem, acc));
}
