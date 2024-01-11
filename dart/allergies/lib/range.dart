extension IntUpTo on int {
  Range upto(int n) => Range(this, n);
}

class Range with Iterable<int> {
  final int start;
  final int end;
  final int step;

  Range(this.start, this.end, [this.step = 1]) {
    // TODO negative step handling.
    assert(step > 0);
  }

  @override
  Iterator<int> get iterator => RangeIterator(start, end, step);
}

class RangeIterator implements Iterator<int> {
  final int start;
  final int end;
  final int step;
  int _current = 0;

  RangeIterator(this.start, this.end, this.step) {
    _current = start - step;
  }

  @override
  int get current => _current;

  @override
  bool moveNext() {
    if (_current < end) {
      _current += step;
      return true;
    }
    return false;
  }
}
