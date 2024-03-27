module ReverseString exposing (reverse)


reverse : String -> String
reverse =
    String.foldl String.cons ""
