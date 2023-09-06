def assert(cond; errmsg):
  if (cond | not) then errmsg | halt_error
  else .
  end ;

(.series | length) as $strlen

| assert($strlen > 0;             "series cannot be empty")
| assert(.sliceLength <= $strlen; "slice length cannot be greater than series length")
| assert(.sliceLength >= 0;       "slice length cannot be negative")
| assert(.sliceLength >  0;       "slice length cannot be zero")

| reduce range($strlen - .sliceLength + 1) as $i (. + {slices: []};
    .slices += [.series[$i:$i + .sliceLength]]
  )
| .slices
