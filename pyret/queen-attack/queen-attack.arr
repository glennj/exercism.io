use context essentials2020

provide:
  data Queen
end

SIZE :: NumInteger = 8

fun is-on-board(n :: NumInteger) -> Boolean:
  (0 <= n) and (n < SIZE)
end

data Queen:
  | queen(row :: NumInteger%(is-on-board), column :: NumInteger%(is-on-board))
    with:
      method can-attack(self, other :: Queen) -> Boolean:
        dx = num-abs(self.row - other.row)
        dy = num-abs(self.column - other.column)
        (dx == 0) or (dy == 0) or (dx == dy)
      end
end
