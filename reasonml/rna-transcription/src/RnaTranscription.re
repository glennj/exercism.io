type dna =
  | A
  | C
  | G
  | T;

type rna =
  | A
  | C
  | G
  | U;

let complement = (d: dna): rna => {
  switch (d) {
  | A => U
  | C => G
  | G => C
  | T => A
  };
};

let toRna = strand => List.map(complement, strand);
