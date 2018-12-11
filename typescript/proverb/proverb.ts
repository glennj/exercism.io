function forWantOfA(...words: string[]): string {
  if (words.length < 2) { throw new Error() }
  return words.map((_, i) => {
    return (i < words.length - 1)
      ? `For want of a ${words[i]} the ${words[i + 1]} was lost.`
      : `And all for the want of a ${words[0]}.`
  }).join('\n')
}

export default forWantOfA
