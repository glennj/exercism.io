open Test
open Bob

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("asking a question", () => {
  equal(
    ~message="asking a question",
    response("Does this cryogenic chamber make me look fat?"),
    "Sure.",
  )
})

test("shouting", () => {equal(~message="shouting", response("WATCH OUT!"), "Whoa, chill out!")})

test("forceful question", () => {
  equal(
    ~message="forceful question",
    response("WHAT'S GOING ON?"),
    "Calm down, I know what I'm doing!",
  )
})

test("silence", () => {equal(~message="silence", response(""), "Fine. Be that way!")})

test("stating something", () => {
  equal(~message="stating something", response("Tom-ay-to, tom-aaaah-to."), "Whatever.")
})

test("asking a numeric question", () => {
  equal(~message="asking a numeric question", response("You are, what, like 15?"), "Sure.")
})

test("asking gibberish", () => {
  equal(~message="asking gibberish", response("fffbbcbeab?"), "Sure.")
})

test("question with no letters", () => {
  equal(~message="question with no letters", response("4?"), "Sure.")
})

test("non-letters with question", () => {
  equal(~message="non-letters with question", response(":) ?"), "Sure.")
})

test("prattling on", () => {
  equal(~message="prattling on", response("Wait! Hang on. Are you going to be OK?"), "Sure.")
})

test("ending with whitespace", () => {
  equal(
    ~message="ending with whitespace",
    response("Okay if like my  spacebar  quite a bit?   "),
    "Sure.",
  )
})

test("multiple line question", () => {
  equal(
    ~message="multiple line question",
    response("\nDoes this cryogenic chamber make\n me look fat?"),
    "Sure.",
  )
})

test("shouting gibberish", () => {
  equal(~message="shouting gibberish", response("FCECDFCAAB"), "Whoa, chill out!")
})

test("shouting numbers", () => {
  equal(~message="shouting numbers", response("1, 2, 3 GO!"), "Whoa, chill out!")
})

test("shouting with special characters", () => {
  equal(
    ~message="shouting with special characters",
    response("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"),
    "Whoa, chill out!",
  )
})

test("shouting with no exclamation mark", () => {
  equal(
    ~message="shouting with no exclamation mark",
    response("I HATE THE DENTIST"),
    "Whoa, chill out!",
  )
})

test("prolonged silence", () => {
  equal(~message="prolonged silence", response("          "), "Fine. Be that way!")
})

test("alternate silence", () => {
  equal(~message="alternate silence", response("\t\t\t\t\t\t\t\t\t\t"), "Fine. Be that way!")
})

test("other whitespace", () => {
  equal(~message="other whitespace", response("\n\r \t"), "Fine. Be that way!")
})

test("talking forcefully", () => {
  equal(~message="talking forcefully", response("Hi there!"), "Whatever.")
})

test("using acronyms in regular speech", () => {
  equal(
    ~message="using acronyms in regular speech",
    response("It's OK if you don't want to go work for NASA."),
    "Whatever.",
  )
})

test("no letters", () => {equal(~message="no letters", response("1, 2, 3"), "Whatever.")})

test("statement containing question mark", () => {
  equal(
    ~message="statement containing question mark",
    response("Ending with ? means a question."),
    "Whatever.",
  )
})

test("starting with whitespace", () => {
  equal(~message="starting with whitespace", response("         hmmmmmmm..."), "Whatever.")
})

test("non-question ending with whitespace", () => {
  equal(
    ~message="non-question ending with whitespace",
    response("This is a statement ending with whitespace      "),
    "Whatever.",
  )
})
