const defaultStudents: string[] = [
  'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred',
  'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry',
]

const plants: {[key: string]: string} = {
  G: 'grass',
  C: 'clover',
  R: 'radishes',
  V: 'violets'
}

export class Garden {
  private plots: Map<string,string[]> = new Map()

  constructor(diagram: string, names: string[] = defaultStudents) {
    const sortedNames = names.slice().sort()

    const rows = diagram.split('\n').map((line) => [...line])
    for (let i = 0; i < rows[0].length / 2; i++) {
      this.plots.set(sortedNames[i], [
        plants[rows[0][2 * i]], plants[rows[0][2 * i + 1]],
        plants[rows[1][2 * i]], plants[rows[1][2 * i + 1]],
      ])
    }
  }

  plants(name: string): string[] | undefined {
    return this.plots.get(name)
  }
}
