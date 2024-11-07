const int size = 8;

class Queen {
  final int rank, file;

  Queen(this.rank, this.file) {
    // not using `assert`: it is not for production usage
    // https://dart.dev/language/error-handling#assert
    if (rank < 0) throw new AssertionError('row not positive');
    if (file < 0) throw new AssertionError('column not positive');
    if (rank >= size) throw new AssertionError('row not on board');
    if (file >= size) throw new AssertionError('column not on board');
  }

  bool canAttack(Queen other) =>
      this.rank == other.rank ||
      this.file == other.file ||
      (this.rank - other.rank).abs() == (this.file - other.file).abs();
}
