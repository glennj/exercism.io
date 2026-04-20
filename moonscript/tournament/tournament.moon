import readlines from require 'pl.utils'
import split from require 'pl.stringx'

process_match = (home, away, result) ->
  switch result
    when 'win'
      home.w += 1
      away.l += 1
    when 'loss'
      home.l += 1
      away.w += 1
    when 'draw'
      home.d += 1
      away.d += 1

standings = (teams) ->
  -- each team represented as the table {name, mp, w, d, l, p}
  s = [{name, (t.w+t.d+t.l), t.w, t.d, t.l, (3*t.w+t.d)} for name, t in pairs teams]
  table.sort s, (a, b) -> a[6] > b[6] or (a[6] == b[6] and a[1] < b[1])
  s

format = (teams) ->
  fmt = "%-31s| %2s | %2s | %2s | %2s | %2s"
  lines = [fmt\format table.unpack team for team in *teams]
  table.insert lines, 1, fmt\format("Team", "MP", "W", "D", "L", "P")
  lines

{
  tally: (filename) ->
    teams = {}
    team = (name) -> teams[name] or {w: 0, d: 0, l: 0}

    for line in *readlines(filename)
      team1, team2, result = table.unpack split line, ';'
      home = team team1
      away = team team2
      process_match home, away, result
      teams[team1] = home
      teams[team2] = away

    format standings teams
}
