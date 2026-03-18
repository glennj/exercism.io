A = 'A'\byte!

{
  rows: (letter) ->
    size = letter\byte! - A + 1

    diamond = {}

    -- top half
    for i = 1, size
      right_half = [' ' for _ = 1, size]
      right_half[i] = string.char A + i - 1
      right = table.concat right_half
      table.insert diamond, right\reverse! .. right\sub(2)

    -- bottom half
    table.insert diamond, diamond[i] for i = size-1, 1, -1

    diamond
}
