class SumOfMultiples {
  int sum(List<int> factors, int limit) {
    var multiples = Set<int>();

    void addMultiples(int factor) {
      var multiple = factor;
      while (multiple < limit) {
        multiples.add(multiple);
        multiple += factor;
      }
    }

    factors.where((factor) => factor > 0).forEach(addMultiples);
    return multiples.fold(0, (sum, multiple) => sum + multiple);
  }
}
