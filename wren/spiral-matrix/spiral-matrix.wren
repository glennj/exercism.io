class SpiralMatrix {
  construct new(size) {
    _size = size
    _matrix = (0...size).map {List.filled(size, null)}.toList
  }

  toList {
    if (_size == 0) {
      return []
    }
    if (_matrix[0][0] == null) {
      populate_
    }
    return _matrix
  }

  populate_ {
    // starting in the top-left cell
    var r = 0
    var c = 0
    
    // setup "delta_row" and "delta_col" so we can advance step by step.
    // These cycles determine the [dr,dc] pairs so we can walk a straight line
    // until we need to turn.
    var dr_cycle = Cycle.new([0, 1, 0, -1])
    var dc_cycle = Cycle.new([1, 0, -1, 0])
    var dr = dr_cycle.next
    var dc = dc_cycle.next

    // We know how many cells to populate
    for (i in (1..(_size * _size))) {
      _matrix[r][c] = i
      // If we hit a boundary or an already populated cell, turn
      if ( [-1, _size].contains(r + dr) ||
           [-1, _size].contains(c + dc) ||
           _matrix[r+dr][c+dc] != null) {
        dr = dr_cycle.next
        dc = dc_cycle.next
      }
      // advance to the next cell
      r = r + dr
      c = c + dc
    }
  }
}

class Cycle {
  construct new(list) {
    _list = list
    _i = 0
  }

  next {
    var elem = _list[_i]
    _i = (_i + 1) % _list.count
    return elem
  }
}