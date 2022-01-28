/* a translation of the Tcl reference solution,
 * which is a translation of the Ruby compact reference solution.
 */

type Dict = { [key: string]: number }

const DIGITS = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]


export function solve(puzzle: string): Dict | undefined {
  const words = puzzle.toUpperCase().split(/\s*(?:\+|==)\s*/)
  const letters = luniq([...words.join('')])

  const firstLetters = words.map(word => word[0])
  const finalLetters = words.map(word => word[word.length - 1])
  const uniqfirst = luniq(firstLetters)
  const uniqlast = luniq(finalLetters)
  const firstNotLast = lsubtract(uniqfirst, uniqlast)
  const uniqrest = luniq(lsubtract(lsubtract(letters, firstLetters), finalLetters))
  const answerLastLetter = finalLetters.pop()!

  for (const lasts of getPermutations(DIGITS, uniqlast.length)) {
    const table = mapDigits(uniqlast, lasts)
    if (uniqfirst.some(first => table[first] === 0))
      continue

    const sum = finalLetters.reduce((sum, f) => sum + table[f], 0)
    if (sum % 10 !== table[answerLastLetter])
      continue

    const digits1 = lsubtract(DIGITS.slice(1), Object.values(table))

    for (const firsts of getPermutations(digits1, firstNotLast.length)) {
      const table2 = mergeDicts(table, mapDigits(firstNotLast, firsts))
      if (uniqfirst.some(first => table2[first] === 0))
        continue

      const digits2 = lsubtract(DIGITS, Object.values(table2))

      for (const rest of getPermutations(digits2, uniqrest.length)) {
        const table3 = mergeDicts(table2, mapDigits(uniqrest, rest))
        const addends = words.map(word => word2num(word, table3))
        const answer = addends.pop()

        if (answer === addends.reduce((sum, n) => sum + n, 0))
          return table3
      }
    }
  }

  return
}

//////////////////////////////////////////////////////////////

// return the unique strings in an array, preserving order
function luniq(items: string[]): string[] {
  const tmp = new Map<string, null>()
  items.forEach(item => tmp.set(item, null))

  const result: string[] = []
  for (const item of tmp.keys())
    result.push(item)
  return result
}


// remove items in array b from array a
function lsubtract<T>(a: T[], b: T[]): T[] {
  const tmp: Map<T, null> = new Map()
  a.forEach(item => tmp.set(item, null))
  b.forEach(item => tmp.delete(item))

  const result: T[] = []
  for (const item of tmp.keys())
    result.push(item)
  return result
}

// "zips" the arrays into an object
//
//    mapDigits(['a', 'b', 'c'], [7, 4, 2])
//    => {a: 7, b: 4, c: 2}
//
function mapDigits(a: string[], b: number[]): Dict {
  const result: Dict = {}
  for (let i = 0; i < a.length; i++)
    result[a[i]] = b[i]
  return result
}

// merge two objects, where the b object overrides duplicate keys:
//
//    mergeDicts({a: 7, b: 4, c: 2}, {d: 5, a: 10})
//    => {a: 10, b: 4, c: 2, d: 5}
//
function mergeDicts(a: Dict, b: Dict): Dict {
  const result: Dict = {}
  for (const obj of [a, b]) {
    Object.entries(obj).forEach(([key, val]) => result[key] = val)
  }
  return result
}

// translate a word to a number given the translation table
//
//    word2num('cab', {a: 7, b: 4, c: 2})
//    => 274
//
function word2num(word: string, table: Dict): number {
  return [...word].reduce((value, c) => value * 10 + table[c], 0)
}

// source https://stackoverflow.com/a/37094178/7552
function getPermutations<T>(a: T[], n: number, s: T[][] = [],t: T[] = []): T[][] {
  return a.reduce((p,c,i,a) => {
      n > 1
        ? getPermutations(a.slice(0,i).concat(a.slice(i+1)), n-1, p, (t.push(c),t))
        : p.push((t.push(c),t).slice(0));
      t.pop();
      return p
  }, s)
}
