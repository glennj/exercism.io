proc isPaired*(input: string): bool =
  var stack: seq[char]
  for c in input:
    case c
      of '[', '{', '(': stack.add c
      of ']', '}', ')':
        if  stack.len == 0 or
            (stack.pop & c) notin ["()", "{}", "[]"]:
          return false
      else: discard
  stack.len == 0
