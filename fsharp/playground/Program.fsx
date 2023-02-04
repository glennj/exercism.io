// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let compare (operand1: int) (cmp: int -> int -> bool) (operand2: int): bool =
    cmp operand1 operand2

let mutable res = true

res <- compare 4 op_Equality 6
printfn $"{res}"

res <- compare 4 op_Equality 4
printfn $"{res}"
