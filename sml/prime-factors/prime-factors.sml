fun primeFactors n =
  let val rec factorize = fn (n:int, f: int, fs: int list) =>
    if f * f > n
    then if n > 1
         then n :: fs
         else fs
    else if n mod f = 0
         then factorize (n div f, f, f :: fs)
         else factorize (n, f + 1, fs)
  in  rev (factorize (n, 2, []))
  end
