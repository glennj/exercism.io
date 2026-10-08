open Test
open WordCount

let dictEqual = (~message=?, a: Dict.t<'a>, b: Dict.t<'a>) => {
  let toSorted = d => {
    let arr = Dict.toArray(d)
    arr->Array.sort(((k1, _), (k2, _)) => (compare(k1, k2) :> float))
    arr
  }
  assertion(~message?, ~operator="dictEqual", (a, b) => toSorted(a) == toSorted(b), a, b)
}

test("count one word", () => {
  dictEqual(~message="count one word", countWords("word"), dict{"word": 1})
})

test("count one of each word", () => {
  dictEqual(
    ~message="count one of each word",
    countWords("one of each"),
    dict{"one": 1, "of": 1, "each": 1},
  )
})

test("multiple occurrences of a word", () => {
  dictEqual(
    ~message="multiple occurrences of a word",
    countWords("one fish two fish red fish blue fish"),
    dict{"one": 1, "fish": 4, "two": 1, "red": 1, "blue": 1},
  )
})

test("handles cramped lists", () => {
  dictEqual(
    ~message="handles cramped lists",
    countWords("one,two,three"),
    dict{"one": 1, "two": 1, "three": 1},
  )
})

test("handles expanded lists", () => {
  dictEqual(
    ~message="handles expanded lists",
    countWords("one,\ntwo,\nthree"),
    dict{"one": 1, "two": 1, "three": 1},
  )
})

test("ignore punctuation", () => {
  dictEqual(
    ~message="ignore punctuation",
    countWords("car: carpet as java: javascript!!&@$%^&"),
    dict{"car": 1, "carpet": 1, "as": 1, "java": 1, "javascript": 1},
  )
})

test("include numbers", () => {
  dictEqual(
    ~message="include numbers",
    countWords("testing, 1, 2 testing"),
    dict{"1": 1, "2": 1, "testing": 2},
  )
})

test("normalize case", () => {
  dictEqual(~message="normalize case", countWords("go Go GO Stop stop"), dict{"go": 3, "stop": 2})
})

test("with apostrophes", () => {
  dictEqual(
    ~message="with apostrophes",
    countWords("'First: don't laugh. Then: don't cry. You're getting it.'"),
    dict{
      "first": 1,
      "don't": 2,
      "laugh": 1,
      "then": 1,
      "cry": 1,
      "you're": 1,
      "getting": 1,
      "it": 1,
    },
  )
})

test("with quotations", () => {
  dictEqual(
    ~message="with quotations",
    countWords("Joe can't tell between 'large' and large."),
    dict{"joe": 1, "can't": 1, "tell": 1, "between": 1, "large": 2, "and": 1},
  )
})

test("substrings from the beginning", () => {
  dictEqual(
    ~message="substrings from the beginning",
    countWords("Joe can't tell between app, apple and a."),
    dict{"joe": 1, "can't": 1, "tell": 1, "between": 1, "app": 1, "apple": 1, "and": 1, "a": 1},
  )
})

test("multiple spaces not detected as a word", () => {
  dictEqual(
    ~message="multiple spaces not detected as a word",
    countWords(" multiple   whitespaces"),
    dict{"multiple": 1, "whitespaces": 1},
  )
})

test("alternating word separators not detected as a word", () => {
  dictEqual(
    ~message="alternating word separators not detected as a word",
    countWords(",\n,one,\n ,two \n 'three'"),
    dict{"one": 1, "two": 1, "three": 1},
  )
})

test("quotation for word with apostrophe", () => {
  dictEqual(
    ~message="quotation for word with apostrophe",
    countWords("can, can't, 'can't'"),
    dict{"can": 1, "can't": 2},
  )
})
