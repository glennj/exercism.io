use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: color-code, colors end

include file("list-helpers.arr")

fun color-code(color):
  index-of(colors(), color)
end

fun colors():
  [list: "black", "brown", "red", "orange", "yellow",
         "green", "blue", "violet", "grey", "white"]
end
