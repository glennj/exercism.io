const MAX = 99
const LIQUID = 'beer'
const WHERE = 'on the wall'

function sing(to: number = MAX, from: number = 0): string {
  return [...new Array(to + 1).keys()]
    .slice(from)
    .reverse()
    .map((i) => verse(i))
    .join('\n')
}

function verse(n: number): string {
  return `${first(n)}\n${second(n)}\n`
}

const first = (n: number): string => {
  const b = bottle(n)
  return `${ucFirst(b)} ${WHERE}, ${b}.`
}

const second = (n: number): string => {
  const b = bottle(n ? n - 1 : MAX)
  const task = n ? `Take ${one(n)} down and pass it around` : 'Go to the store and buy some more'
  return `${task}, ${b} ${WHERE}.`
}

const bottle = (n: number): string => {
  return `${n || 'no more'} bottle${n !== 1 ? 's' : ''} of ${LIQUID}`
}

const one = (n: number): string => n > 1 ? 'one' : 'it'

const ucFirst = (s: string): string => s.substring(0, 1).toUpperCase() + s.substring(1)

export default { verse, sing }
