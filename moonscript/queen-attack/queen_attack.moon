class Queen
  new: (row, column) =>
    assert 0 <= row and row < 8, 'invalid position'
    assert 0 <= column and column < 8, 'invalid position'
    @r = row
    @c = column

  can_attack: (other) =>
    dr = math.abs(@r - other.r)
    dc = math.abs(@c - other.c)
    dr == 0 or dc == 0 or dr == dc
