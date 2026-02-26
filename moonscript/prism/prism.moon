import p from require 'moon'
-- puts = p
puts = (...) ->

-- ------------------------------------------------------------
round_to = (n, pts) ->
  mult = 10 ^ pts
  math.floor(n * mult + 0.5) / mult


normalize_angle = (a) ->
  math.fmod(360 + math.fmod(a, 360), 360)

approx_equal = (a, b, epsilon=0.0001) ->
  math.abs(a - b) <= epsilon

-- ------------------------------------------------------------
is_inline = (line, prism) ->
  dx = prism.x - line.x
  dy = prism.y - line.y
  m = math.tan line.theta

  puts {id: prism.id, :dy, :dx, :m}

  if approx_equal math.abs(math.fmod line.angle, 180), 90
    dx == 0
  else
    approx_equal dy, dx * math.tan line.theta


next_prism = (line, prisms) ->
  local next_one
  min = math.huge
  for p in *prisms
    dx = p.x - line.x
    dy = p.y - line.y
    projection = dx * math.cos(line.theta) + dy * math.sin(line.theta)
    if projection > 0 and projection < min
      min = projection
      next_one = p

  next_one


{
  findSequence: (start, prisms) ->
    line = start
    seq = {}

    while true
      line.angle = normalize_angle line.angle
      line.theta = math.rad line.angle

      puts {:line}


      inline_prisms = [p for p in *prisms when is_inline line, p]
      puts {:inline_prisms}
      p = next_prism line, inline_prisms
      puts {next: p}
      break if not p

      table.insert seq, p.id
      line = x: p.x, y: p.y, angle: math.fmod(line.angle + p.angle, 360)

    seq
}
