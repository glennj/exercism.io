def spiral:
  # initialze a n x n matrix with all zeroes
  def _init_matrix:
    . as $n
    | ([] | until(length == $n; . + [0])) as $row
    | [] | until(length == $n; . + [$row])
  ;

  def _populate:
    if .i > .n * .n then
      .matrix
    else
      .matrix[.x][.y] = .i
      | if  .x + .dx < 0 or .x + .dx == .n or
            .y + .dy < 0 or .y + .dy == .n or
            .matrix[.x + .dx][.y + .dy] != 0
        then
          . + {dx: .dy, dy: -.dx}
        end
      | .x += .dx
      | .y += .dy
      | .i += 1
      | _populate
    end
  ;

  # create the "state object" and pass into the recursive helper func
  {i: 1, n: ., matrix: _init_matrix, x: 0, y: 0, dx: 0, dy: 1}
  | _populate
;

.size | spiral