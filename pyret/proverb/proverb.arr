provide: recite end

import file("string-helpers.arr") as SH
include from SH:
  string-format
end

fun recite(items):
  rec stanzas = lam(xs, lines):
    ask: 
      | xs.length() < 2 then: lines
      | otherwise:
          stanzas(
            xs.drop(1),
            lines.push(string-format("For want of a {1} the {2} was lost.", xs.take(2)))
          )
    end
  end

  cases(List) items:
    | empty => [list:]
    | link(first, _) =>
        stanzas(items, [list:])
          .push("And all for the want of a " + first + ".")
          .reverse()
  end
end
