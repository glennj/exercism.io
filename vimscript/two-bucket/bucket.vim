let s:bucketPrototype = {}

function s:bucketPrototype.IsEmpty()
  return self.amount == 0
endfunction

function s:bucketPrototype.IsFull()
  return self.amount == self.size
endfunction

function s:bucketPrototype.Empty()
  let self.amount = 0
endfunction

function s:bucketPrototype.Fill()
  let self.amount = self.size
endfunction

function s:bucketPrototype.Pour(other)
  let quantity = min([self.amount, a:other.size - a:other.amount])
  let self.amount -= quantity
  let a:other.amount += quantity
endfunction

lockvar! s:bucketPrototype

function Bucket(name, size)
  let bucket = s:bucketPrototype->copy()
  let bucket.name = a:name
  let bucket.size = a:size
  let bucket.amount = 0
  return bucket
endfunction
