def tally:
  # a team's data is an array: [name, mp, w, d, l p]
  def init(name): .[name] //= [name, 0, 0, 0, 0, 0] ;

  def win:  .[1] += 1 | .[2] += 1 | .[5] += 3 ;
  def draw: .[1] += 1 | .[3] += 1 | .[5] += 1 ;
  def lose: .[1] += 1 | .[4] += 1 | .[5] += 0 ;

  reduce (.[] | split(";")) as [$home, $away, $result] ({};
    init($home)
    | init($away)
    | if   $result == "win"  then .[$home] |= win  | .[$away] |= lose
      elif $result == "loss" then .[$home] |= lose | .[$away] |= win
      elif $result == "draw" then .[$home] |= draw | .[$away] |= draw
      end
  )

  | to_entries
  | map(.value)
;

def rpad(width): tostring + " " * width | .[:width] ;
def lpad(width): " " * width + tostring | .[-width:] ;

def to_table:
  [.[0] | rpad(30)] + [.[1:][] | lpad(2)]
  | join(" | ")
;

.matches 
| tally 
| sort_by(-1 * .[-1], .[0])
| [["Team", "MP", "W", "D", "L", "P"]] + .
| map(to_table)[]
