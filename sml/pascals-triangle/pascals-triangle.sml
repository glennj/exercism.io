local
  (* TODO memoization *)
  fun factorial 0 = 1
    | factorial n = n * factorial (n - 1)

  fun choose (n, k) = (factorial n) div (factorial k * factorial (n - k))
  infix choose
in
  fun rows (n: int): int list list =
    List.tabulate (n, fn i => (List.tabulate (i + 1, fn j => i choose j)))
end
