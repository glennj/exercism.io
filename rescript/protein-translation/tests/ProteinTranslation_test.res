open Test
open ProteinTranslation

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Empty RNA sequence results in no proteins", () => {equal(~message="Empty RNA sequence results in no proteins", proteins(""), Some([]))})

test("Methionine RNA sequence", () => {equal(~message="Methionine RNA sequence", proteins("AUG"), Some([Methionine]))})

test("Phenylalanine RNA sequence 1", () => {equal(~message="Phenylalanine RNA sequence 1", proteins("UUU"), Some([Phenylalanine]))})

test("Phenylalanine RNA sequence 2", () => {equal(~message="Phenylalanine RNA sequence 2", proteins("UUC"), Some([Phenylalanine]))})

test("Leucine RNA sequence 1", () => {equal(~message="Leucine RNA sequence 1", proteins("UUA"), Some([Leucine]))})

test("Leucine RNA sequence 2", () => {equal(~message="Leucine RNA sequence 2", proteins("UUG"), Some([Leucine]))})

test("Serine RNA sequence 1", () => {equal(~message="Serine RNA sequence 1", proteins("UCU"), Some([Serine]))})

test("Serine RNA sequence 2", () => {equal(~message="Serine RNA sequence 2", proteins("UCC"), Some([Serine]))})

test("Serine RNA sequence 3", () => {equal(~message="Serine RNA sequence 3", proteins("UCA"), Some([Serine]))})

test("Serine RNA sequence 4", () => {equal(~message="Serine RNA sequence 4", proteins("UCG"), Some([Serine]))})

test("Tyrosine RNA sequence 1", () => {equal(~message="Tyrosine RNA sequence 1", proteins("UAU"), Some([Tyrosine]))})

test("Tyrosine RNA sequence 2", () => {equal(~message="Tyrosine RNA sequence 2", proteins("UAC"), Some([Tyrosine]))})

test("Cysteine RNA sequence 1", () => {equal(~message="Cysteine RNA sequence 1", proteins("UGU"), Some([Cysteine]))})

test("Cysteine RNA sequence 2", () => {equal(~message="Cysteine RNA sequence 2", proteins("UGC"), Some([Cysteine]))})

test("Tryptophan RNA sequence", () => {equal(~message="Tryptophan RNA sequence", proteins("UGG"), Some([Tryptophan]))})

test("STOP codon RNA sequence 1", () => {equal(~message="STOP codon RNA sequence 1", proteins("UAA"), Some([]))})

test("STOP codon RNA sequence 2", () => {equal(~message="STOP codon RNA sequence 2", proteins("UAG"), Some([]))})

test("STOP codon RNA sequence 3", () => {equal(~message="STOP codon RNA sequence 3", proteins("UGA"), Some([]))})

test("Sequence of two protein codons translates into proteins", () => {equal(~message="Sequence of two protein codons translates into proteins", proteins("UUUUUU"), Some([Phenylalanine, Phenylalanine]))})

test("Sequence of two different protein codons translates into proteins", () => {equal(~message="Sequence of two different protein codons translates into proteins", proteins("UUAUUG"), Some([Leucine, Leucine]))})

test("Translate RNA strand into correct protein list", () => {equal(~message="Translate RNA strand into correct protein list", proteins("AUGUUUUGG"), Some([Methionine, Phenylalanine, Tryptophan]))})

test("Translation stops if STOP codon at beginning of sequence", () => {equal(~message="Translation stops if STOP codon at beginning of sequence", proteins("UAGUGG"), Some([]))})

test("Translation stops if STOP codon at end of two-codon sequence", () => {equal(~message="Translation stops if STOP codon at end of two-codon sequence", proteins("UGGUAG"), Some([Tryptophan]))})

test("Translation stops if STOP codon at end of three-codon sequence", () => {equal(~message="Translation stops if STOP codon at end of three-codon sequence", proteins("AUGUUUUAA"), Some([Methionine, Phenylalanine]))})

test("Translation stops if STOP codon in middle of three-codon sequence", () => {equal(~message="Translation stops if STOP codon in middle of three-codon sequence", proteins("UGGUAGUGG"), Some([Tryptophan]))})

test("Translation stops if STOP codon in middle of six-codon sequence", () => {equal(~message="Translation stops if STOP codon in middle of six-codon sequence", proteins("UGGUGUUAUUAAUGGUUU"), Some([Tryptophan, Cysteine, Tyrosine]))})

test("Sequence of two non-STOP codons does not translate to a STOP codon", () => {equal(~message="Sequence of two non-STOP codons does not translate to a STOP codon", proteins("AUGAUG"), Some([Methionine, Methionine]))})

test("Unknown amino acids, not part of a codon, can't translate", () => {equal(~message="Unknown amino acids, not part of a codon, can't translate", proteins("XYZ"), None)})

test("Incomplete RNA sequence can't translate", () => {equal(~message="Incomplete RNA sequence can't translate", proteins("AUGU"), None)})

test("Incomplete RNA sequence can translate if valid until a STOP codon", () => {equal(~message="Incomplete RNA sequence can translate if valid until a STOP codon", proteins("UUCUUCUAAUGGU"), Some([Phenylalanine, Phenylalanine]))})
