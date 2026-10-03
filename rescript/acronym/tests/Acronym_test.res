open Test
open Acronym

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("basic", () => {equal(~message="basic", abbreviate("Portable Network Graphics"), "PNG")})

test("lowercase words", () => {equal(~message="lowercase words", abbreviate("Ruby on Rails"), "ROR")})

test("punctuation", () => {equal(~message="punctuation", abbreviate("First In, First Out"), "FIFO")})

test("all caps word", () => {equal(~message="all caps word", abbreviate("GNU Image Manipulation Program"), "GIMP")})

test("punctuation without whitespace", () => {equal(~message="punctuation without whitespace", abbreviate("Complementary metal-oxide semiconductor"), "CMOS")})

test("very long abbreviation", () => {equal(~message="very long abbreviation", abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"), "ROTFLSHTMDCOALM")})

test("consecutive delimiters", () => {equal(~message="consecutive delimiters", abbreviate("Something - I made up from thin air"), "SIMUFTA")})

test("apostrophes", () => {equal(~message="apostrophes", abbreviate("Halley's Comet"), "HC")})

test("underscore emphasis", () => {equal(~message="underscore emphasis", abbreviate("The Road _Not_ Taken"), "TRNT")})
