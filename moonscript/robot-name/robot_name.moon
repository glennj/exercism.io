-- all names
ALPHA = [c for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'\gmatch '.']
NAMES = ['%s%s%03d'\format a, b, c for a in *ALPHA for b in *ALPHA for c = 0,999]

-- shuffle it
for i = #NAMES, 2, -1
  j = math.random i
  NAMES[i], NAMES[j] = NAMES[j], NAMES[i]
  

class Robot
  @index: 0
  
  @reset_names: => @index = 0

  new: =>
    @reset!
  
  reset: =>
    @@index += 1
    assert @@index <= #NAMES, 'all names taken'
    @nam = NAMES[@@index]

  name: => @nam

