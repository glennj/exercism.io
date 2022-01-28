export function count(phrase: string): Map<string, number> {
  const map = new Map()
  phrase
    .trim()
    .toLowerCase()
    .split(/\s+/)
    .forEach((word) => {
      map.set(word, 1 + (map.get(word) || 0))
    })
  return map
}
