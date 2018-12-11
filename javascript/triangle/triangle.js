/* eslint no-multi-spaces: ["error", { ignoreEOLComments: true }] */

export default class Triangle {
  constructor(x = 0, y = 0, z = 0) {
    const [a, b, c] = [x, y, z].sort((i, j) => i - j);  // sort numerically
    this.sides = [a, b, c];
    if (a + b < c || a <= 0) {
      this.type = null;                                 // should throw here
    } else if (a === b && a === c) {
      this.type = 'equilateral';
    } else if (a === b || a === c || b === c) {
      this.type = 'isosceles';
    } else {
      this.type = 'scalene';
    }
  }

  kind() {
    if (this.type === null) throw new Error('illegal triangle');
    return this.type;
  }
}


/* community
 *
 * dispatch table:

    const rules = [
      {
        rule: ({a,b,c}) => a === 0 || b === 0 || c === 0,
        type: () => { throw new Error('Triangle can\'t have 0 size side') }
      },
      {
        rule: ({a,b,c}) => a <= 0 || b <= 0 || c <= 0,
        type: () => { throw new Error('Triangle can\'t have negative or zero side') }
      },
      {
        rule: ({a,b,c}) => a + b <= c || a + c <= b || b + c < a,
        type: () => { throw new Error('Triangle is impossible') }
      },
      {
        rule: ({a,b,c}) => a === b && a === c,
        type: () => 'equilateral'
      },
      {
        rule: ({a,b,c}) => a === b || a === c || b === c,
        type: () => 'isosceles'
      },
      {
        rule: ({a,b,c}) => a !== b && a !== c && b !== c,
        type: () => 'scalene'
      },
      {
        rule: () => true,
        type: () => { throw new Error('We\'re missing a rule') }
      },
    ]

    class Triangle {
      constructor(a,b,c){
        this.sides = {a,b,c}
      }

      kind() {
        return rules
          .find(({rule}) => rule(this.sides))
          .type()
      }
    }

    export default Triangle
 *
 */
