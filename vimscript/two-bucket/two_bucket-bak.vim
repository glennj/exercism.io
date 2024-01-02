execute 'source' expand('<sfile>:p:h') . '/bucket.vim'

function TwoBucket(input) abort
  s:validateInput(a:input)

  let start = Bucket('one', a:input.bucketOne)
  let other = Bucket('two', a:input.bucketOne)
  if a:input.startBucket != 'one' | let [start, other] = [other, start] | endif

  let game = {'start': start, 'other': other, 'goal': a:input.goal}

"  function game.Measure()
"    let moves = 0
"
"    eval self.start.Fill()
"    let moves = moves + 1
"
"    if self.other.size == self.goal
"      eval self.other.Fill()
"      let moves += 1
"    endif
"
"    while 1
"      if self.start.amount == self.goal
"        return s:result(moves, self.start.name, self.other.amount)
"      elseif self.other.amount == self.goal
"        return s:result(moves, self.other.name, self.start.amount)
"      endif
"
"      if      self.start.IsEmpty() | eval self.start.Fill()
"      elseif  self.other.IsFull()  | eval self.other.Empty()
"      else                         | eval self.start.Pour(self.other)
"      endif
"      let moves += 1
"    endwhile
"  endfunction

  return game
endfunction

function s:validateInput(input) abort
  if a:input.goal > max([a:input.bucketOne, a:input.bucketTwo])
    throw "impossible"
  endif
  let gcd = s:GCD(a:input.bucketOne, a:input.bucketTwo)
  if gcd != 1 && a:input.goal % gcd != 0
    throw "impossible"
  endif
endfunction

function s:GCD(x, y)
  return a:y == 0 ? a:x : s:GCD(a:y, a:x % a:y)
endfunction

function s:result(winner, loser, moves)
  return {'moves': a:moves, 'goalBucket': a:winner.name, 'otherBucket': a:loser.amount}
endfunction
