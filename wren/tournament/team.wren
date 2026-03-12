import "./string-utils" for StringUtils as Str

class Team {
  construct new(name) {
    _name = name
    _wins = 0
    _draws = 0
    _losses = 0
  }

  name { _name }
  matches { _wins + _draws + _losses }
  points { 3 * _wins + _draws }

  win { _wins = _wins + 1 }
  draw { _draws = _draws + 1 }
  lose { _losses = _losses + 1 }

  stats { [_name, matches, _wins, _draws, _losses, points] }

  // for sorting: a team sorts first if it has more points, or
  // if the points are equal and it has a "smaller" name lexically
  <(other) { points > other.points || (points == other.points && Str.lt(name, other.name)) }
}
