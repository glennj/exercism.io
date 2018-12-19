type Actor = { who: string, what?: string }

class House {
  static actors: Actor[] = [
    { who: 'house that Jack built.' },
    { who: 'malt', what: 'lay in' },
    { who: 'rat', what: 'ate' },
    { who: 'cat', what: 'killed' },
    { who: 'dog', what: 'worried' },
    { who: 'cow with the crumpled horn', what: 'tossed' },
    { who: 'maiden all forlorn', what: 'milked' },
    { who: 'man all tattered and torn', what: 'kissed' },
    { who: 'priest all shaven and shorn', what: 'married' },
    { who: 'rooster that crowed in the morn', what: 'woke' },
    { who: 'farmer sowing his corn', what: 'kept' },
    { who: 'horse and the hound and the horn', what: 'belonged to' },
  ]

  static verse(n: number): string[] {
    const lines = [`This is the ${this.actors[n - 1].who}`]
    for (let i = n - 1; i >= 1; i -= 1) {
      lines.push(`that ${this.actors[i].what} the ${this.actors[i - 1].who}`)
    }
    return lines
  }

  static verses(from: number, to: number): string[] {
    const range = Array.from({length: to - from + 1}, (_, i) => i + from)
    const lyrics = range.reduce(
      (lines, n) => lines.concat('', this.verse(n)),
      new Array<string>()
    )
    return lyrics.slice(1)
  }
}

export default House
