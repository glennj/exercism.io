use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: nucleotide-counts end

include string-dict

fun nucleotide-counts(strand):
  string-explode(strand)
    .foldl(
      lam(nucleotide, count):
        n = count.get(nucleotide)
        cases(Option) n:
          | none => raise("Invalid nucleotide in strand")
          | else => count.set(nucleotide, n.value + 1)
        end
      end,
      [string-dict: "A", 0, "C", 0, "G", 0, "T", 0])
end
