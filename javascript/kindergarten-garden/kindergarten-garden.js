/* eslint-disable  object-curly-newline */

const students = [
  'Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred',
  'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry',
];

const plants = { G: 'grass', C: 'clover', R: 'radishes', V: 'violets' };

export class Garden {
  constructor(diagram, names = students) {
    const sortedNames = names.sort();
    const rows = diagram.split('\n').map(line => Array.from(line));
    this.plantsFor = {};
    for (let i = 0; i < rows[0].length / 2; i += 1) {
      const key = sortedNames[i];
      this.plantsFor[key] = [
        plants[rows[0][2 * i]], plants[rows[0][2 * i + 1]],
        plants[rows[1][2 * i]], plants[rows[1][2 * i + 1]],
      ];
    }
  }

  plants(name) {
    return this.plantsFor[name];
  }
}
