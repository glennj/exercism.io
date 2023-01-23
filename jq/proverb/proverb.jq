import "lib/arrays" as Array;

.strings
| if length == 0
    then []
    else
      reduce Array::each_cons(2) as $pair ([];
        . + ["For want of a \($pair[0]) the \($pair[1]) was lost."]
      )
      + ["And all for the want of a \(first)."]
    end
