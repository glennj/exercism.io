export function acronym(sentence) {
  return sentence
    .replaceAll("-", " ")
    .replaceAll(/[^a-z ]/gi, "")
    .split(/\s+/)
    .map((word) => word[0])
    .join("")
    .toLocaleUpperCase();
}
