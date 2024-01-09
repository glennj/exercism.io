extension IntBitAt on int {
  int bitAt(int offset) => (this >> offset) & 1;
  bool isBitSetAt(int offset) => bitAt(offset) == 1;
}
