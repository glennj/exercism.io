def assert(cond; msg): if cond then . else (msg | halt_error) end;

def satellite:
  if .preorder == [] then
    {}
  else
    .preorder[0] as $root
    | (.inorder | index($root)) as $idx
    | {
        v: $root,
        l: ({preorder: .preorder[1:$idx+1], inorder: .inorder[:$idx]}   | satellite),
        r: ({preorder: .preorder[$idx+1:],  inorder: .inorder[$idx+1:]} | satellite)
      }
  end
;

assert((.preorder | sort == unique); "traversals must contain unique items")
| assert((.preorder | length) == (.inorder | length); "traversals must have the same length")
| assert((.preorder | sort) == (.inorder | sort); "traversals must have the same elements")
| satellite
