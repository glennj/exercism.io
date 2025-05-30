const d2r = { G: "C", C: "G", T: "A", A: "U" };

export const dnaToRna = (dna) => [...dna].map((d) => d2r[d]).join("");
