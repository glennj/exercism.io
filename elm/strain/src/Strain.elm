module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    keeper predicate [] list


keeper : (a -> Bool) -> List a -> List a -> List a
keeper predicate acc list =
    case list of
        [] ->
            List.reverse acc

        first :: rest ->
            if predicate first then
                keeper predicate (first :: acc) rest

            else
                keeper predicate acc rest


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    keep (predicate >> not) list
