normalize_angle = (a) ->
  math.fmod(360 + math.fmod(a, 360), 360)


is_inline = (line, prism) ->
  -- determine the perpendicular distance of the point from the line
  dx = prism.x - line.x
  dy = prism.y - line.y
  dist = dx * math.sin(line.theta) - dy * math.cos(line.theta)
  -- if it's on the line, dist should be zero.
  -- this epsilon value determined by trial and error
  return math.abs(dist) <= 0.0011


next_prism = (line, prisms) ->
  nxt = nil
  min = math.huge
  for p in *prisms
    dx = p.x - line.x
    dy = p.y - line.y
    projection = dx * math.cos(line.theta) + dy * math.sin(line.theta)
    if projection > 0 and projection < min
      min = projection
      nxt = p
  nxt


{
  findSequence: (start, prisms) ->
    line = start
    seq = {}

    while true
      line.angle = normalize_angle line.angle
      line.theta = math.rad line.angle

      inline_prisms = [p for p in *prisms when is_inline line, p]
      p = next_prism line, inline_prisms
      break if not p

      table.insert seq, p.id
      line = x: p.x, y: p.y, angle: line.angle + p.angle

    seq
}
