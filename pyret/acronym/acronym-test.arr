use context starter2024

include file("acronym.arr")

check "basic":
  abbreviate("Portable Network Graphics") is "PNG"
end

check "lowercase words":
  abbreviate("Ruby on Rails") is "ROR"
end

check "punctuation":
  abbreviate("First In, First Out") is "FIFO"
end

check "all caps word":
  abbreviate("GNU Image Manipulation Program") is "GIMP"
end

check "punctuation without whitespace":
  abbreviate("Complementary metal-oxide semiconductor") is "CMOS"
end

check "very long abbreviation":
  abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me") is "ROTFLSHTMDCOALM"
end

check "consecutive delimiters":
  abbreviate("Something - I made up from thin air") is "SIMUFTA"
end

check "apostrophes":
  abbreviate("Halley's Comet") is "HC"
end

check "underscore emphasis":
  abbreviate("The Road _Not_ Taken") is "TRNT"
end
