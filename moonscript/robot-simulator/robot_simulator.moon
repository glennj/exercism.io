turns =
  left:
    north: 'west'
    east: 'north'
    south: 'east'
    west: 'south'
  right:
    north: 'east'
    east: 'south'
    south: 'west'
    west: 'north'

advancement =
  north: {dx: 0, dy: 1}
  east:  {dx: 1, dy: 0}
  south: {dx: 0, dy: -1}
  west:  {dx: -1, dy: 0}


class Robot
  new: (params) =>
    @_x = params.x
    @_y = params.y
    @_dir = params.direction

  x: => @_x
  y: => @_y
  direction: => @_dir

  move: (instructions) =>
    for i in instructions\gmatch '.'
      switch i
        when 'L' then @_dir = turns.left[@_dir]
        when 'R' then @_dir = turns.right[@_dir]
        when 'A'
          @_x += advancement[@_dir].dx
          @_y += advancement[@_dir].dy
