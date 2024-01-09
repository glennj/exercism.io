extension IntUpTo on int {
  Range upto(int n) => Range(this, n);
}

class Range with Iterable<int> {
  final int start;
  final int end;
  final int step;

  Range(this.start, this.end, [this.step = 1]) {
    // TODO negative step handling.
    assert(this.step > 0);
  }

  @override
  Iterator<int> get iterator => RangeIterator(this.start, this.end, this.step);
}

class RangeIterator implements Iterator<int> {
  final int start;
  final int end;
  final int step;
  int _i = 0;

  RangeIterator(this.start, this.end, this.step) {
    this._i = this.start - this.step;
  }

  @override
  int get current => this._i;

  @override
  bool moveNext() {
    if (this._i < this.end) {
      this._i += this.step;
      return true;
    }
    return false;
  }
}
