FLOWER = 99

to_n = (char) ->
  switch char
    when ' ' then 0
    when '*' then FLOWER
    else tonumber char

to_s = (n) ->
  switch n
    when 0 then ' '
    when FLOWER then '*'
    else tostring n

{
  annotate: (garden) ->
    a = [ [to_n char for char in row\gmatch '.'] for row in *garden]
    in_range = (x, y) -> (1 <= x and x <= #a) and (1 <= y and y <= #a[1])

    for i = 1, #a
      for j = 1, #a[i]
        if a[i][j] != FLOWER
          for di = -1,1
            for dj = -1,1
              if in_range(i+di, j+dj) and a[i+di][j+dj] == FLOWER
                a[i][j] += 1

    [table.concat [to_s n for n in *row] for row in *a]
}
