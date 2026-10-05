open Test
open Pangram

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("empty sentence", () => {equal(~message="empty sentence", isPangram(""), false)})

test("perfect lower case", () => {equal(~message="perfect lower case", isPangram("abcdefghijklmnopqrstuvwxyz"), true)})

test("only lower case", () => {equal(~message="only lower case", isPangram("the quick brown fox jumps over the lazy dog"), true)})

test("missing the letter 'x'", () => {equal(~message="missing the letter 'x'", isPangram("a quick movement of the enemy will jeopardize five gunboats"), false)})

test("missing the letter 'h'", () => {equal(~message="missing the letter 'h'", isPangram("five boxing wizards jump quickly at it"), false)})

test("with underscores", () => {equal(~message="with underscores", isPangram("the_quick_brown_fox_jumps_over_the_lazy_dog"), true)})

test("with numbers", () => {equal(~message="with numbers", isPangram("the 1 quick brown fox jumps over the 2 lazy dogs"), true)})

test("missing letters replaced by numbers", () => {equal(~message="missing letters replaced by numbers", isPangram("7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog"), false)})

test("mixed case and punctuation", () => {equal(~message="mixed case and punctuation", isPangram("\"Five quacking Zephyrs jolt my wax bed.\""), true)})

test("a-m and A-M are 26 different characters but not a pangram", () => {equal(~message="a-m and A-M are 26 different characters but not a pangram", isPangram("abcdefghijklm ABCDEFGHIJKLM"), false)})
