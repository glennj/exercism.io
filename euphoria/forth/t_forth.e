include std/unittest.e 

with trace
include forth.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("numbers just get pushed onto the stack", {1,2,3,4,5}, evaluate("1 2 3 4 5"))
test_equal("pushes negative numbers onto the stack", {-1,-2,-3,-4,-5}, evaluate("-1 -2 -3 -4 -5"))

test_equal("can add two numbers", {3}, evaluate("1 2 +"))
test_equal("errors if there is nothing on the stack", "empty stack", evaluate("+"))
test_equal("errors if there is only one value on the stack", "only one value on the stack", evaluate("1 +"))

test_equal("can subtract two numbers", {-1}, evaluate("3 4 -"))
test_equal("errors if there is nothing on the stack", "empty stack", evaluate("-"))
test_equal("errors if there is only one value on the stack", "only one value on the stack", evaluate("1 -"))
 
test_equal("can multiply two numbers", {8}, evaluate("2 4 *"))
test_equal("errors if there is nothing on the stack", "empty stack", evaluate("*"))
test_equal("errors if there is only one value on the stack", "only one value on the stack", evaluate("1 *"))

test_equal("can divide two numbers", {4}, evaluate("12 3 /"))
test_equal("performs integer division",{2},evaluate("8 3 /"))
test_equal("errors if dividing by zero","divide by zero",evaluate("4 0 /"))
test_equal("errors if there is nothing on the stack", "empty stack", evaluate("/"))
test_equal("errors if there is only one value on the stack", "only one value on the stack", evaluate("1 /"))

test_equal("addition and subtraction",{-1},evaluate("1 2 + 4 -"))
test_equal("multiplication and division",{2},evaluate("2 4 * 3 /"))

test_equal("copies value on the stack", {1,1}, evaluate("1 dup"))
test_equal("copies the top value on the stack", {1,2,2}, evaluate("1 2 dup"))
test_equal("errors if there is nothing on the stack","empty stack",evaluate("dup"))

test_equal("removes the top value on the stack if it is the only one",{},evaluate("1 drop"))
test_equal("errors if there is nothing on the stack","empty stack",evaluate("drop"))

test_equal("swaps the top two values on the stack if they are the only ones",{2,1},evaluate("1 2 swap"))
test_equal("swaps the top two values on the stack if they are not the only ones",{1, 3, 2},evaluate("1 2 3 swap"))
test_equal("errors if there is nothing on the stack","empty stack",evaluate("swap"))
test_equal("errors if there is only one value on the stack","only one value on the stack",evaluate("1 swap"))

test_equal("copies the second element if there are only two",{1, 2, 1},evaluate("1 2 over"))
test_equal("copies the second element if there are more than two",{1,2,3,2},evaluate("1 2 3 over"))
test_equal("errors if there is nothing on the stack","empty stack",evaluate("over"))
test_equal("errors if there is only one value on the stack","only one value on the stack",evaluate("1 over"))

test_equal("can consist of built-in words", {1,1,1}, evaluate({": dup-twice dup dup ;","1 dup-twice"}))
test_equal("execute in the right order",{1,2,3},evaluate({
    ": countup 1 2 3 ;",
    "countup"
  }))
test_equal("can override other user-defined words",{1,1,1},evaluate({
    ": foo dup ;",
    ": foo dup dup ;",
    "1 foo"
  }))
test_equal( "can override built-in operators",{12},evaluate({
    ": + * ;",
    "3 4 +"
  }))
test_equal( "can use different words with the same name",{5,6},evaluate({
    ": foo 5 ;",
    ": bar foo ;",
    ": foo 6 ;",
    "bar foo"}))
test_equal("cannot redefine non-negative numbers","illegal operation",evaluate({": 1 2 ;"}))    
test_equal("cannot redefine negative numbers","illegal operation",evaluate({": -1 2 ;"}))    
test_equal("errors if executing a non-existent word","undefined operation",evaluate({"foo"}))
test_equal("only defines locally",{{0},{2}},{evaluate({": + - ;", "1 1 +"}),evaluate({"1 1 +"})})
test_equal("DUP is case-insensitive",{1,1,1,1},evaluate({"1 DUP Dup dup"}))
test_equal("DROP is case-insensitive",{1},evaluate({"1 2 3 4 DROP Drop drop"}))
test_equal("SWAP is case-insensitive",{2,3,4,1},evaluate({"1 2 SWAP 3 Swap 4 swap"}))
test_equal("OVER is case-insensitive",{1,2,1,2,1},evaluate({"1 2 OVER Over over"}))
test_equal( "user-defined words are case-insensitive",{1,1,1,1},evaluate({
  ": foo dup ;",
  "1 FOO Foo foo"}))
test_equal("definitions are case-insensitive",{1,1,1,1},evaluate({
  ": SWAP DUP Dup dup ;",
  "1 swap"}))

test_equal("multiple user-defined words", {5,6}, evaluate(": foo 5 ; : bar foo ; : foo 6 ; bar foo"))

test_report() 
