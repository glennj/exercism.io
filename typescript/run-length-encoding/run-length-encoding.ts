function encode(text: string): string {
  return text.replace(
    /(.)\1+/g,
    (match) => { return `${match.length}${match[0]}` }
  )
}

function decode(text: string): string {
  return text.replace(
    /(\d+)(\D)/g,
    (_, num, char) => { return char.repeat(num) }
  )
}

export default { encode, decode }
