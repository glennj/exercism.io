type Animal = {
  name: string
  tag?: string
  that?: string
  apexPredator?: boolean
}

const chain: Animal[] = [{name: ''},
  { name: 'fly' },
  { name: 'spider', that: ' that wriggled and jiggled and tickled inside her',
                    tag: 'It wriggled and jiggled and tickled inside her.\n' },
  { name: 'bird',   tag: 'How absurd to swallow a bird!\n' },
  { name: 'cat',    tag: 'Imagine that, to swallow a cat!\n' },
  { name: 'dog',    tag: 'What a hog, to swallow a dog!\n' },
  { name: 'goat',   tag: 'Just opened her throat and swallowed a goat!\n' },
  { name: 'cow',    tag: 'I don\'t know how she swallowed a cow!\n' },
  { name: 'horse',  tag: 'She\'s dead, of course!\n', apexPredator: true },
]

const iKnow = (animal: Animal): string => {
  return `I know an old lady who swallowed a ${animal.name}.\n${animal.tag || ''}`
}
const hunt = (predator: Animal, prey: Animal): string => {
  return `She swallowed the ${predator.name}`
       + ` to catch the ${prey.name}${prey.that || ''}.\n`
}
const ending = 'I don\'t know why she swallowed the fly. Perhaps she\'ll die.\n'

const seq = (from: number, to: number): number[] => {
  return [...new Array(to + 1).keys()].slice(from)
}

const verses = (from: number, to: number): string => {
  return seq(from, to).map((i) => verse(i)).join('\n')
}

const verse = (n: number): string => {
  const animal = chain[n]
  let v = iKnow(animal)
  if (!animal.apexPredator) {
    seq(2, n).reverse().forEach((i) => v += hunt(chain[i], chain[i - 1]))
    v += ending
  }
  return v
}

export default { verse, verses }
