extension on String {
  String get reversed => split('').reversed.join('');
  String skip(int n) => (length <= n) ? '' : substring(n);
}

class Diamond {
  final a = 'A'.codeUnitAt(0);

  List<String> rows(String letter) {
    int size = letter.codeUnitAt(0) - a + 1;
    assert(1 <= size && size <= 26);

    /* a list of strings holding the right-hand half of the rows
     * for the top half of the diamond.
     */
    var topRightQuadrant = List.generate(size, (i) {
      var row = List.filled(size, ' ');
      row[i] = String.fromCharCode(a + i);
      return row.join('');
    });

    /* complete the left-hand half of the rows 
     */
    var topHalf =
        topRightQuadrant.map((row) => row.skip(1).reversed + row).toList();

    return topHalf + topHalf.reversed.skip(1).toList();
  }
}
