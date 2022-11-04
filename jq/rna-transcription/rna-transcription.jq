def toRna:
  split("")
  | map({G: "C", C: "G", T: "A", A: "U"}[.] // "")
  | join("");