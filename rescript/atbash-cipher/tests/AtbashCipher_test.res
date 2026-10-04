open Test
open AtbashCipher

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("encode yes", () => {equal(~message="encode yes", encode("yes"), "bvh")})

test("encode no", () => {equal(~message="encode no", encode("no"), "ml")})

test("encode OMG", () => {equal(~message="encode OMG", encode("OMG"), "lnt")})

test("encode spaces", () => {equal(~message="encode spaces", encode("O M G"), "lnt")})

test("encode mindblowingly", () => {
  equal(~message="encode mindblowingly", encode("mindblowingly"), "nrmwy oldrm tob")
})

test("encode numbers", () => {
  equal(~message="encode numbers", encode("Testing,1 2 3, testing."), "gvhgr mt123 gvhgr mt")
})

test("encode deep thought", () => {
  equal(~message="encode deep thought", encode("Truth is fiction."), "gifgs rhurx grlm")
})

test("encode all the letters", () => {
  equal(
    ~message="encode all the letters",
    encode("The quick brown fox jumps over the lazy dog."),
    "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt",
  )
})

test("decode exercism", () => {equal(~message="decode exercism", decode("vcvix rhn"), "exercism")})

test("decode a sentence", () => {
  equal(
    ~message="decode a sentence",
    decode("zmlyh gzxov rhlug vmzhg vkkrm thglm v"),
    "anobstacleisoftenasteppingstone",
  )
})

test("decode numbers", () => {
  equal(~message="decode numbers", decode("gvhgr mt123 gvhgr mt"), "testing123testing")
})

test("decode all the letters", () => {
  equal(
    ~message="decode all the letters",
    decode("gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"),
    "thequickbrownfoxjumpsoverthelazydog",
  )
})

test("decode with too many spaces", () => {
  equal(~message="decode with too many spaces", decode("vc vix    r hn"), "exercism")
})

test("decode with no spaces", () => {
  equal(
    ~message="decode with no spaces",
    decode("zmlyhgzxovrhlugvmzhgvkkrmthglmv"),
    "anobstacleisoftenasteppingstone",
  )
})
