use context starter2024

provide:
  string-pad-right,
  string-trim-right
end

fun string-pad-right(str, len):
  doc: "add spaces to make the string the specified length"
  str + string-repeat(" ", len - string-length(str))
where:
  string-pad-right("hello", 7) is "hello  "
  string-pad-right(" hello", 7) is " hello "
  string-pad-right("hello", 5) is "hello"
  string-pad-right("hello", 3) is "hello"
  string-pad-right("hello world", 13) is "hello world  "
end


fun string-trim-right(s):
  doc: "remove all spaces from the end of a string"
  ask:
    | string-length(s) == 0 then: s
    | string-ends-with(s, " ") then: string-trim-right(string-substring(s, 0, string-length(s) - 1))
    | otherwise: s
  end
where:
  string-trim-right("hello world  ") is "hello world"
  string-trim-right("   hello    ") is "   hello"
end
