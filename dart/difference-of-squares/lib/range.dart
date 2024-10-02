// ref https://dart.dev/libraries/dart-core#iteration

class Range extends Iterable<int> {
  final int start;
  final int end;
  final int step;

  Range(this.start, this.end, [this.step = 1]) {
    // TODO downwards ranges
    if (this.start > this.end)
      throw ArgumentError('Start cannot be greater than end.');
    if (this.step <= 0) throw ArgumentError('Step must be positive.');
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
    this._i += this.step;
    return this._i <= this.end;
  }
}
