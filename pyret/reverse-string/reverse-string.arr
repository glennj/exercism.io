use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: reversed end

fun reversed(text):
  string-explode(text).foldl({(rev, char): rev + char}, "")
end
