cleanupname(name) = replace(name, "-" => " ") |> strip

#firstletter(name) = cleanupname(name) |> first |> string
firstletter = string ∘ first ∘ cleanupname

initial = (s -> "$s.") ∘ uppercase ∘ firstletter

couple(name1, name2) = "❤ $(initial(name1))  +  $(initial(name2)) ❤"
