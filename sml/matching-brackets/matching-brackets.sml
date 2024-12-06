local
  val parens = [ (#"(", #")")
               , (#"[", #"]")
               , (#"{", #"}")
               ]
  
  fun isOpener c     = List.exists (fn (a, _) => a = c) parens
  fun isCloser c     = List.exists (fn (_, b) => b = c) parens
  fun isMatch (a, b) = List.exists (fn pair => pair = (a, b)) parens
  
  fun isBalanced' stack []    = null stack
    | isBalanced' stack (c::cs) =
        if isOpener c
        then isBalanced' (c :: stack) cs
        else if not (isCloser c)
             then isBalanced' stack cs
             else if not (null stack) andalso isMatch (hd stack, c)
                  then isBalanced' (tl stack) cs
                  else false
in
  val isBalanced = (isBalanced' []) o explode
end