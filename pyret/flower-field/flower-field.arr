use context starter2024

import arrays as A

provide: annotate end

FLOWER = 99
SPACE = 0

fun annotate(garden):
  ask:
    | is-empty(garden) then: [list: ]
    | (garden.length() == 1) and (garden.get(0) == "") then: [list: ""]
    | otherwise:
        garden
          ^ make-matrix(_)
          ^ increment-neighbours(_)
          ^ to-garden(_)
  end
end

# make an array of arrays
fun make-matrix(garden :: List<String>) -> Array<Array<Number>>:
  garden.map(lam(row):
    string-explode(row).map(lam(c): if c == "*": FLOWER else: SPACE end end)
      ^ A.array-from-list(_)
  end)
    ^ A.array-from-list(_)
end

fun increment-neighbours(mtx :: Array<Array<Number>>):
  is-flower = lam(r, c): mtx.get-now(r).get-now(c) == FLOWER end
  in-row-range = lam(r): (0 <= r) and (r < mtx.length()) end
  in-col-range = lam(c): (0 <= c) and (c < mtx.get-now(0).length()) end
  inc = lam(r, c):
    current = mtx.get-now(r).get-now(c)
    mtx.get-now(r).set-now(c, current + 1)
  end

  _ =
    range(0, mtx.length()).each(lam(r):
      range(0, mtx.get-now(r).length()).each(lam(c):
        when not(is-flower(r, c)):
          [list: r - 1, r, r + 1].each(lam(rr):
            [list: c - 1, c, c + 1].each(lam(cc):
              when in-row-range(rr) and in-col-range(cc) and is-flower(rr, cc):
                inc(r, c)
              end
            end)
          end)
        end
      end)
    end)
  mtx
end

fun to-garden(mtx :: Array<Array<Number>>):
  mtx.fold(lam(garden, row, _):
    r = row.fold(lam(str, n, _): str + ask:
          | n == FLOWER then: "*"
          | n == SPACE then: " "
          | otherwise: num-to-string(n)
        end
    end, "", 0)
    garden.push(r)
  end, [list: ], 0).reverse()
end
