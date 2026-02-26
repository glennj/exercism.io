
{
  spiral_matrix: (size) ->
    mtx = [{} for _ = 1,size]

    ok = (n) -> 1 <= n and n <= size

    x, y   = 1, 1
    dx, dy = 0, 1

    for i = 1, size * size
      mtx[x][y] = i
      if not (ok(x + dx) and ok(y + dy) and mtx[x + dx][y + dy] == nil)
        dx, dy = dy, -dx    -- turn right
      x += dx
      y += dy

    mtx
}
