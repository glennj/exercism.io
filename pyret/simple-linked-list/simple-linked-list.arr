use context essentials2020

provide:
  data LinkedList
end

data LinkedList:
  | empty-list
    with:
      method length(self): 0 end,
      method get-tail(self): self end,
      method to-list(self): [list:] end

  | linked-list(head, tail :: LinkedList)
    with:
      method length(self) -> NumInteger:
        1 + self.tail.length()
      end,
      method get-head(self):
        self.head
      end,
      method get-tail(self) -> LinkedList:
        self.tail
      end,
      method to-list(self) -> List:
        self.tail.to-list() + [list: self.head]
      end

  sharing:
    method push(self, value) -> LinkedList:
      linked-list(value, self)
    end,

    method reversed(self) -> LinkedList:
      rec reverser = lam(shadow list, rev):
        ask:
          | is-empty-list(list) then: rev
          | otherwise: reverser(list.get-tail(), linked-list(list.get-head(), rev))
        end
      end
      reverser(self, empty-list)
    end

end
