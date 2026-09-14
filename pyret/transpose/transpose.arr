use context starter2024

import max from math
import file("string-helpers.arr") as S

provide: transpose end

fun transpose(lines):
  ask:
    | is-empty(lines) then: [list: ]
    | otherwise:
        max-width = max(lines.map(string-length))
        padded = lines.map(lam(row): S.string-pad-right(row, max-width) end)
        transposed = range(0, max-width).map(lam(i):
          padded.foldl(lam(row, col): col + string-substring(row, i, i + 1) end, "")
        end)

        transposed
          .foldr(
            lam(row, {result; width}):
              trimmed = S.string-trim-right(row)
              w = num-max(width, string-length(trimmed))
              {result.push(S.string-pad-right(trimmed, w)); w}
            end,
            {[list: ]; 0}
          ).{0}
  end
end
