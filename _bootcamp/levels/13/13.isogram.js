function isLetter(lower) {
  const code = lower.charCodeAt(0);
  return 97 <= code && code <= 122;
}

export function isIsogram(string) {
  const letters = string.toLowerCase().split("").filter(isLetter);
  return letters.length === new Set(letters).size;
}
