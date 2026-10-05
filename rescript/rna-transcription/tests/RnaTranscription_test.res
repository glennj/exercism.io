open Test
open RnaTranscription

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Empty RNA sequence", () => {equal(~message="Empty RNA sequence", toRna(""), "")})

test("RNA complement of cytosine is guanine", () => {
  equal(~message="RNA complement of cytosine is guanine", toRna("C"), "G")
})

test("RNA complement of guanine is cytosine", () => {
  equal(~message="RNA complement of guanine is cytosine", toRna("G"), "C")
})

test("RNA complement of thymine is adenine", () => {
  equal(~message="RNA complement of thymine is adenine", toRna("T"), "A")
})

test("RNA complement of adenine is uracil", () => {
  equal(~message="RNA complement of adenine is uracil", toRna("A"), "U")
})

test("RNA complement", () => {
  equal(~message="RNA complement", toRna("ACGTGGTCTTAA"), "UGCACCAGAAUU")
})
