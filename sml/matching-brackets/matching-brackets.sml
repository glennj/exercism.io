val parens = [ (#"(", #")")
             , (#"[", #"]")
             , (#"{", #"}")
             ]

fun isOpener c     = List.exists (fn (a, _) => a = c) parens
fun isCloser c     = List.exists (fn (_, b) => b = c) parens
fun isMatch (a, b) = List.exists (fn pair => pair = (a, b)) parens

fun isBalanced' ([], stack)    = null stack
  | isBalanced' (c::cs, stack) =
      if isOpener c
      then isBalanced' (cs, c :: stack)
      else if not (isCloser c)
           then isBalanced' (cs, stack)
           else if not (null stack) andalso isMatch (hd stack, c)
                then isBalanced' (cs, tl stack)
                else false

fun isBalanced s = isBalanced' (String.explode s, [])
