let encode = plaintext => {
  plaintext->String.replaceRegExpBy1Unsafe(
    /(.)\1+/g, 
    (~match, ~group1, ~offset as _, ~input as _) =>
      match->String.length->Int.toString + group1)
}

let decode = ciphertext => {
  ciphertext->String.replaceRegExpBy2Unsafe(
    /(\d+)(\D)/g,
    (~match as _, ~group1, ~group2, ~offset as _, ~input as _) =>
      group2->String.repeat(group1->Int.fromString->Option.getUnsafe))
}
