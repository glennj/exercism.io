Deltas = {
  {-1, -1}, {-1, 0}, {-1, 1},
   {0, -1},          {0, 1},
   {1, -1},  {1, 0}, {1, 1},
}

-- ---------------------------------------------------------------
class WordSearch
  new: (grid) =>
    @grid = [ [c for c in row\gmatch '.'] for row in *grid]
    @height = #@grid
    @width = #@grid[1]

  find: (words) =>
    -- a table comprehension
    { word, @findWord(word) for word in *words }
    -- recall, assigning nil to a table key removes the key from the table.

  findWord: (word) =>
    for r = 1, @height
      for c = 1, @width
        if word\sub(1, 1) == @grid[r][c]
          delta = @check word, r, c, dr, dc
          if delta
            return {
              start: { column: c, row: r },
              end: {
                column: c + (#word - 1) * delta.dc,
                row: r + (#word - 1) * delta.dr
              }
            }
    nil

  check: (word, r, c) =>
    for {dr, dc} in *Deltas
      found = true
      for i = 2, #word
        nr = r + (i - 1) * dr
        nc = c + (i - 1) * dc
        if nr < 1 or nr > @height or nc < 1 or nc > @width or @grid[nr][nc] != word\sub(i, i)
          found = false
          break
      if found
        return {:dr, :dc}
    nil
