use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: matrix end

fun matrix(input :: String) -> Object:
  mtx = input
        ^ string-split-all(_, "\n")
        ^ map(
            lam(line):
              line
              ^ string-split-all(_, " ")
              ^ map({(w): string-to-number(w).value}, _)
            end, _)
  # return an object, compare with high-scores.
  {
    matrix: mtx,

    method row(self, n) -> List:
      self.matrix.get(n - 1)
    end,

    method column(self, n) -> List:
      range(0, self.matrix.length())
      ^ map({(m): self.matrix.get(m).get(n - 1)}, _)
    end
  }
end
