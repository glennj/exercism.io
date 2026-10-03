open Test
open HelloWorld

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Say Hi!", () => {equal(~message="Say Hi!", hello(), "Hello, World!")})
