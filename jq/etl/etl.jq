.legacy
| to_entries

| map({key: (.value[] | ascii_downcase), value: (.key | tonumber)})
    #
    # The "key" expression outputs a stream of values.
    # That means the object creation will be evaluated once per stream entry.
    # If the "value" expression were to output multiple values, 
    # then we'd end up with the cross-product list of objects.

| sort_by(.key)     # plain `sort` works too
| from_entries
