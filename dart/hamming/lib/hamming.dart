class Hamming {
  int distance(String strand1, String strand2) {
    if (strand1.length != strand2.length) {
      throw ArgumentError('strands must be of equal length');
    }

    int helper(String s1, String s2, int dist) {
      if (s1.isEmpty) return dist;
      var cmp = s1[0] == s2[0] ? 0 : 1;
      return helper(s1.substring(1), s2.substring(1), dist + cmp);
    }

    return helper(strand1, strand2, 0);
  }
}
