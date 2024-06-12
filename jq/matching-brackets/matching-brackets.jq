def isBalanced:
  if .value == "" then (.stack | length) == 0
  else
    .value[0:1] as $char
    | .value |= .[1:]
    | if   ($char | IN("[", "{", "("))       then .stack += [$char] | isBalanced
      elif ($char | IN("]", "}", ")") | not) then isBalanced
      else
        if {"]":"[", "}":"{", ")":"("}[$char] == .stack[-1]
        then .stack |= .[:-1] | isBalanced
        else false
        end
      end
  end
;

. + {stack: []} | isBalanced