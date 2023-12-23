use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: keep, discard end

fun keep(sequence, predicate):
  rec keeper = lam(seq, kept):
    cases(List) seq:
      | empty => kept.reverse()
      | link(x, xs) => if predicate(x):
                         keeper(xs, kept.push(x))
                       else:
                         keeper(xs, kept)
                       end
    end
  end

  keeper(sequence, [list: ])
end

fun discard(sequence, predicate):
  keep(sequence, lam(x): not(predicate(x)) end)
end
