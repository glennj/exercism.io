let isPangram = input => {
  let alphabet = ref("abcdefghijklmnopqrstuvwxyz")
  for i in 0 to String.length(input) - 1 {
    let char = input->String.charAt(i)->String.toLowerCase
    alphabet := alphabet.contents->String.replace(char, "")
  }
  alphabet.contents->String.isEmpty
}
