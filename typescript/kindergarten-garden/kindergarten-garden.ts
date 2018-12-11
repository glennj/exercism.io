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

class Garden {
  // declare an index signature for the class so we can assign properties dynamically
  [key: string]: string[]

  constructor(diagram: string, names: string[] = defaultStudents) {
    const sortedNames = names.sort()
    const rows = diagram.split('\n').map((line) => [...line])

    for (let i = 0; i < rows[0].length / 2; i++) {
      const key = sortedNames[i].toLowerCase()

      // assign this student's crops
      this[key] = [
        plants[rows[0][2 * i]], plants[rows[0][2 * i + 1]],
        plants[rows[1][2 * i]], plants[rows[1][2 * i + 1]],
      ]
    }
  }
}

export default Garden
