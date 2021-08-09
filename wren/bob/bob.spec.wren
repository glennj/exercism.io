import "wren-testie/testie" for Testie, Expect
import "./bob" for Bob

Testie.test("Bob") { |do, skip|
  do.test("stating something") {
    var result = Bob.hey("Tom-ay-to, tom-aaaah-to.")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("shouting") {
    var result = Bob.hey("WATCH OUT!")
    Expect.value(result).toEqual("Whoa, chill out!")
  }

  do.test("shouting gibberish") {
    var result = Bob.hey("FCECDFCAAB")
    Expect.value(result).toEqual("Whoa, chill out!")
  }

  do.test("asking a question") {
    var result = Bob.hey("Does this cryogenic chamber make me look fat?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("asking a numeric question") {
    var result = Bob.hey("You are, what, like 15?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("asking gibberish") {
    var result = Bob.hey("fffbbcbeab?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("talking forcefully") {
    var result = Bob.hey("Let's go make out behind the gym!")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("using acronyms in regular speech") {
    var result = Bob.hey("It's OK if you don't want to go to the DMV.")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("forceful question") {
    var result = Bob.hey("WHAT THE HELL WERE YOU THINKING?")
    Expect.value(result).toEqual("Calm down, I know what I'm doing!")
  }

  do.test("shouting numbers") {
    var result = Bob.hey("1, 2, 3 GO!")
    Expect.value(result).toEqual("Whoa, chill out!")
  }

  do.test("no letters") {
    var result = Bob.hey("1, 2, 3")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("question with no letters") {
    var result = Bob.hey("4?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("shouting with special characters") {
    var result = Bob.hey("ZOMG THE \%^*@#$(*^ ZOMBIES ARE COMING!!11!!1!")
    Expect.value(result).toEqual("Whoa, chill out!")
  }

  do.test("shouting with no exclamation mark") {
    var result = Bob.hey("I HATE YOU")
    Expect.value(result).toEqual("Whoa, chill out!")
  }

  do.test("statement containing question mark") {
    var result = Bob.hey("Ending with a ? means a question.")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("non-letters with question") {
    var result = Bob.hey(":) ?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("prattling on") {
    var result = Bob.hey("Wait! Hang on. Are you going to be OK?")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("silence") {
    var result = Bob.hey("")
    Expect.value(result).toEqual("Fine. Be that way!")
  }

  do.test("prolonged silence") {
    var result = Bob.hey("          ")
    Expect.value(result).toEqual("Fine. Be that way!")
  }

  do.test("alternate silence") {
    var result = Bob.hey("\t\t\t\t\t\t\t\t\t\t")
    Expect.value(result).toEqual("Fine. Be that way!")
  }

  do.test("multiple line question") {
    var result = Bob.hey("\nDoes this cryogenic chamber make me look fat?\nno")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("starting with whitespace") {
    var result = Bob.hey("         hmmmmmmm...")
    Expect.value(result).toEqual("Whatever.")
  }

  do.test("ending with whitespace") {
    var result = Bob.hey("Okay if like my  spacebar  quite a bit?   ")
    Expect.value(result).toEqual("Sure.")
  }

  do.test("other whitespace") {
    var result = Bob.hey("\n\r \t")
    Expect.value(result).toEqual("Fine. Be that way!")
  }

  do.test("non-question ending with whitespace") {
    var result = Bob.hey("This is a statement ending with whitespace      ")
    Expect.value(result).toEqual("Whatever.")
  }
}
