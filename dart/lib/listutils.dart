class ListUtils {
  /** Return a list containing a range of ints from 0 (inclusive)
   *  up to `n` (exclusive)
   *
   *    ListUtils.range(5)  // => [0, 1, 2, 3, 4]
   */
  static List<int> range(int n) {
    return List.generate(n, (i) => i);
  }

  /** Return true if lists contain the same elements in the same order.
   */
  static bool equal(List list1, List list2) {
    if (list1.length != list2.length) {
      return false;
    }
    return range(list1.length).every((i) => list1[i] == list2[i]);
  }
}
