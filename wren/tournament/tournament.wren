import "./team" for Team
import "./string-utils" for StringUtils as Str

class Tournament {
  static tally(rows) { this.new(rows).tally }

  construct new(rows) {
    _matches = rows.map {|row| row.split(";")}
    _teams = {}
  }

  tally {
    process
    return format
  }

  process {
    for (match in _matches) {
      if (!_teams.containsKey(match[0])) _teams[match[0]] = Team.new(match[0])
      if (!_teams.containsKey(match[1])) _teams[match[1]] = Team.new(match[1])
      var home = _teams[match[0]]
      var away = _teams[match[1]]

      if (match[2] == "win") {
        home.win
        away.lose
      } else if (match[2] == "draw") {
        home.draw
        away.draw
      } else {
        home.lose
        away.win
      }
    }
  }

  format {
    var standings = [ formatRow(["Team", "MP", "W", "D", "L", "P"]) ]
    for (team in _teams.values.toList.sort()) {
      var stats = team.stats.map {|i| i.toString}.toList
      standings.add(formatRow(stats))
    }
    return standings
  }

  formatRow(items) {
    var row = [Str.padEnd(items[0], 30)] + items[1..-1].map {|i| Str.padStart(i, 2)}
    return row.join(" | ")
  }
}
