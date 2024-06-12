def turn(rotation):
  if   .direction == "north" and rotation == "right" then .direction = "east"
  elif .direction == "north" and rotation == "left"  then .direction = "west"
  elif .direction == "east"  and rotation == "right" then .direction = "south"
  elif .direction == "east"  and rotation == "left"  then .direction = "north"
  elif .direction == "south" and rotation == "right" then .direction = "west"
  elif .direction == "south" and rotation == "left"  then .direction = "east"
  elif .direction == "west"  and rotation == "right" then .direction = "north"
  elif .direction == "west"  and rotation == "left"  then .direction = "south"
  end
;

def advance:
  if   .direction == "north" then .position.y += 1
  elif .direction == "east"  then .position.x += 1
  elif .direction == "south" then .position.y -= 1
  elif .direction == "west"  then .position.x -= 1
  end
;

def simulate:
  if (.instructions // "") == ""
  then .robot
  else
    .instructions[0:1] as $instruction
    | if   $instruction == "R" then .robot |= turn("right")
      elif $instruction == "L" then .robot |= turn("left")
      elif $instruction == "A" then .robot |= advance
      end
    | .instructions |= .[1:]
    | simulate
  end
;

simulate