use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: square-of-sum, sum-of-squares, difference-of-squares end

fun f0(number, f1, f2):
  range(1, number + 1)
  ^ fold({(sum, n): sum + f1(n)}, 0, _)
  ^ f2(_)
end

ident  = {(n): n}
square = {(n): n * n}

square-of-sum  = f0(_, ident, square)
sum-of-squares = f0(_, square, ident)

difference-of-squares = lam(n): num-abs(square-of-sum(n) - sum-of-squares(n)) end
