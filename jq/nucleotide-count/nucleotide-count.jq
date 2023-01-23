reduce (.strand / "")[] as $nucleotide ({A: 0, C: 0, G: 0, T: 0};
  if has($nucleotide)
    then .[$nucleotide] += 1
    else "Invalid nucleotide in strand" | halt_error
  end
)
