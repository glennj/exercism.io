use context starter2024

include file("wordy.arr")

check "just a number":
  answer("What is 5?") is 5
end

check "just a zero":
  answer("What is 0?") is 0
end

check "just a negative number":
  answer("What is -123?") is -123
end

check "addition":
  answer("What is 1 plus 1?") is 2
end

check "addition with a left hand zero":
  answer("What is 0 plus 2?") is 2
end

check "addition with a right hand zero":
  answer("What is 3 plus 0?") is 3
end

check "more addition":
  answer("What is 53 plus 2?") is 55
end

check "addition with negative numbers":
  answer("What is -1 plus -10?") is -11
end

check "large addition":
  answer("What is 123 plus 45678?") is 45801
end

check "subtraction":
  answer("What is 4 minus -12?") is 16
end

check "multiplication":
  answer("What is -3 multiplied by 25?") is -75
end

check "division":
  answer("What is 33 divided by -3?") is -11
end

check "multiple additions":
  answer("What is 1 plus 1 plus 1?") is 3
end

check "addition and subtraction":
  answer("What is 1 plus 5 minus -2?") is 8
end

check "multiple subtraction":
  answer("What is 20 minus 4 minus 13?") is 3
end

check "subtraction then addition":
  answer("What is 17 minus 6 plus 3?") is 14
end

check "multiple multiplication":
  answer("What is 2 multiplied by -2 multiplied by 3?") is -12
end

check "addition and multiplication":
  answer("What is -3 plus 7 multiplied by -2?") is -8
end

check "multiple division":
  answer("What is -12 divided by 2 divided by -3?") is 2
end

check "unknown operation":
  answer("What is 52 cubed?") raises "unknown operation"
end

check "Non math question":
  answer("Who is the President of the United States?") raises "unknown operation"
end

check "reject problem missing an operand":
  answer("What is 1 plus?") raises "syntax error"
end

check "reject problem with no operands or operators":
  answer("What is?") raises "syntax error"
end

check "reject two operations in a row":
  answer("What is 1 plus plus 2?") raises "syntax error"
end

check "reject two numbers in a row":
  answer("What is 1 plus 2 1?") raises "syntax error"
end

check "reject postfix notation":
  answer("What is 1 2 plus?") raises "syntax error"
end

check "reject prefix notation":
  answer("What is plus 1 2?") raises "syntax error"
end
