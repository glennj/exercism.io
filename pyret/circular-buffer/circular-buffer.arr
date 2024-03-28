use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide-types *
provide: make-buffer end

data CircularBuffer:
  | buffer(buff :: Array<NumInteger>, count :: NumNonNegative, rp :: NumNonNegative, wp :: NumNonNegative)

    with:
      method read(self):
        ask:
          | self.count == 0 then: raise("empty buffer")
          | otherwise:
              value = self.buff.get-now(self.rp)
              rp = num-modulo(self.rp + 1, self.buff.length())
              new-buff = buffer(self.buff, self.count - 1, rp, self.wp)
              {value; new-buff}
        end
      end,

      method write(self, value):
        ask:
          | self.count == self.buff.length() then: raise("full buffer")
          | otherwise:
              block:
                self.buff.set-now(self.wp, value)
                wp = num-modulo(self.wp + 1, self.buff.length())
                buffer(self.buff, self.count + 1, self.rp, wp)
              end
        end
      end,

      method overwrite(self, value):
        ask:
          | self.count == self.buff.length() then:
              {_; read-buff} = self.read()
              read-buff.write(value)
          | otherwise:
              self.write(value)
        end
      end,

      method clear(self):
        buffer(self.buff, 0, 0, 0)
      end
end

fun make-buffer(capacity):
  buffer(array-of(0, capacity), 0, 0, 0)
end
