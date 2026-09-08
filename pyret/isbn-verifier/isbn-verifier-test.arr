use context starter2024

include file("isbn-verifier.arr")

check "valid isbn":
  is-valid("3-598-21508-8") is true
end

check "invalid isbn check digit":
  is-valid("3-598-21508-9") is false
end

check "valid isbn with a check digit of 10":
  is-valid("3-598-21507-X") is true
end

check "check digit is a character other than X":
  is-valid("3-598-21507-A") is false
end

check "invalid check digit in isbn is not treated as zero":
  is-valid("4-598-21507-B") is false
end

check "invalid character in isbn is not treated as zero":
  is-valid("3-598-P1581-X") is false
end

check "X is only valid as a check digit":
  is-valid("3-598-2X507-9") is false
end

check "only one check digit is allowed":
  is-valid("3-598-21508-96") is false
end

check "X is not substituted by the value 10":
  is-valid("3-598-2X507-5") is false
end

check "valid isbn without separating dashes":
  is-valid("3598215088") is true
end

check "isbn without separating dashes and X as check digit":
  is-valid("359821507X") is true
end

check "isbn without check digit and dashes":
  is-valid("359821507") is false
end

check "too long isbn and no dashes":
  is-valid("3598215078X") is false
end

check "too short isbn":
  is-valid("00") is false
end

check "isbn without check digit":
  is-valid("3-598-21507") is false
end

check "check digit of X should not be used for 0":
  is-valid("3-598-21515-X") is false
end

check "empty isbn":
  is-valid("") is false
end

check "input is 9 characters":
  is-valid("134456729") is false
end

check "invalid characters are not ignored after checking length":
  is-valid("3132P34035") is false
end

check "invalid characters are not ignored before checking length":
  is-valid("3598P215088") is false
end

check "input is too long but contains a valid isbn":
  is-valid("98245726788") is false
end

