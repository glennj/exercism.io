use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: square-root end

# sigh, builtin rational numbers
fun int-div(x, y): num-floor(x / y) end

fun square-root(number):
  doc: ```
    Using the Binary numeral system (base 2) from Wikipedia
    https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
  ```

  fun sqrt(n, x, b):
    ask:
      | b == 0 then: x
      | n >= (x + b) then: sqrt(n - (x + b), int-div(x, 2) + b, int-div(b, 4))
      | otherwise: sqrt(n, int-div(x, 2), int-div(b, 4))
    end
  end

  sqrt(number, 0, num-expt(4, int-div(num-log(number), num-log(4))))
end
