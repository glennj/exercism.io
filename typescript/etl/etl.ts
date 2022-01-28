type Old = {[key: string]: string[]}
type New = {[key: string]: number}

export function transform (oldObj: Old): New {
  const transformed: New = {}

  Object.entries(oldObj)
    .forEach(([scoreStr, letters]) => {
      const score = Number(scoreStr)
      letters.forEach(letter => {
        transformed[letter.toLowerCase()] = score
      })
    })

  return transformed
}
