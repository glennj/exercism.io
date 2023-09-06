# using the builtin `flatten` is cheating...

def flatten_it:
  reduce (.[] | values) as $elem ([];
    . + if ($elem | type) == "array" then ($elem | flatten_it) else [$elem] end
  );

flatten_it
