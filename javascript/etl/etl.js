export function transform(old) {

  /*
  //straightfowardly

  const result = {}
  for (const [points, letters] of Object.entries(old)) {
    const value = parseInt(points, 10)
    letters.forEach(letter => {
      result[letter.toLowerCase()] = value
    })
  }
  return result
  */

  // functionally

  const transformEntries = (transformed, [points, letters]) => {
    const value = Number.parseInt(points, 10)
    return transformed.concat(letters.map(l => {
      return [l.toLowerCase(), value]
    }))
  }

  const entriesToObject = (result, [key, value]) => {
    result[key] = value
    return result
  }

  return Object.entries(old)
    .reduce(transformEntries, [])
    .reduce(entriesToObject, {})
}
