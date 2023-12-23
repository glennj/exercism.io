use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: distance end

import lists as L
include equality

fun distance(s1, s2):
  if not(string-length(s1) == string-length(s2)):
    raise("Strands must be of equal length.")
  else:
    map2(equal-always, string-explode(s1), string-explode(s2))
    ^ filter(not, _)
    ^ L.length(_)
  end
end
