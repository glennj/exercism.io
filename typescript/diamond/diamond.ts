class Diamond {
  static codeBase = 64    // ascii 'A' minus 1

  makeDiamond(char: string): string {
    const n = char.charCodeAt(0) - Diamond.codeBase
    let rows = []
    for (let i = 1; i <= n; i += 1) {
      // make the string for the upper-right quadrant: "A   ", " B  ", etc
      const a = new Array(n).fill(' ')
      a[i - 1] = String.fromCharCode(Diamond.codeBase + i)
      // reverse it and drop concat with it minus first char to make a row
      rows.push( [...a].reverse().concat( a.slice(1) ).join('') )
    }
    // we now have the top half. Add the bottom half
    rows = rows.concat( [...rows].reverse().slice(1) )
    // and join it all together
    return rows.map((s) => `${s}\n`).join('')
  }
}

export default Diamond
