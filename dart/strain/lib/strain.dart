/* More generics shenanigans.
   https://dart.dev/language/generics#using-generic-methods
 */

class Strain {
  List<T> keep<T>(List<T> elements, bool predicate(T element)) {
    var result = <T>[];
    for (final elem in elements)
      if (predicate(elem))
        result.add(elem);
    return result;
  }
  
  List<T> discard<T>(List<T> elements, bool predicate(T element)) {
    return keep(elements, (elem) => !predicate(elem));
  }
}