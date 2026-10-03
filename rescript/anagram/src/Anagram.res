let findAnagrams = (subject, candidates) => {
  let toKey = word => {
    let chars = word->String.split("")
    Array.sort(chars, String.compare)
    chars
  }
  let subjLower = String.toLowerCase(subject)
  let subjKey = toKey(subjLower)
  let anagrams = Array.filter(candidates, candidate => {
    let candLower = String.toLowerCase(candidate)
    subjLower !== candLower && subjKey == toKey(candLower)
  })
  if Array.isEmpty(anagrams) {
    None
  } else {
    Some(anagrams)
  }
}
