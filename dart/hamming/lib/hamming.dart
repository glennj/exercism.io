class Hamming {
  int distance(String strand1, String strand2) {
    if (strand1.length != strand2.length) {
      throw ArgumentError('strands must be of equal length');
    }
    return _doDistance(strand1, strand2, 0);
  }

  int _doDistance(String s1, String s2, int dist) {
    if (s1.isEmpty) return dist;
    return _doDistance(s1.substring(1), s2.substring(1), dist + (s1[0] == s2[0] ? 0 : 1));
  }
}
