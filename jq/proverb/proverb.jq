.strings
| . as $items
| [
    foreach range(1; length) as $i (null; null;
      "For want of a \($items[$i - 1]) the \($items[$i]) was lost."
    )
    ,
    if length > 0 then "And all for the want of a \(first)." else empty end
  ]
