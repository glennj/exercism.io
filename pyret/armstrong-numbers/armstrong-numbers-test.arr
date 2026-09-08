use context starter2024

include file("armstrong-numbers.arr")

check "Zero is an Armstrong number":
  is-armstrong-number(0) is true
end

check "Single-digit numbers are Armstrong numbers":
  is-armstrong-number(5) is true
end

check "There are no two-digit Armstrong numbers":
  is-armstrong-number(10) is false
end

check "Three-digit number that is an Armstrong number":
  is-armstrong-number(153) is true
end

check "Three-digit number that is not an Armstrong number":
  is-armstrong-number(100) is false
end

check "Four-digit number that is an Armstrong number":
  is-armstrong-number(9474) is true
end

check "Four-digit number that is not an Armstrong number":
  is-armstrong-number(9475) is false
end

check "Seven-digit number that is an Armstrong number":
  is-armstrong-number(9926315) is true
end

check "Seven-digit number that is not an Armstrong number":
  is-armstrong-number(9926314) is false
end

check "Armstrong number containing seven zeroes":
  is-armstrong-number(186709961001538790100634132976990) is true
end

check "The largest and last Armstrong number":
  is-armstrong-number(115132219018763992565095597973971522401) is true
end
