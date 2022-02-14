import { Garden } from './kindergarten-garden'

describe('partial Garden', () => {
  it('garden with single student', () => {
    expect(new Garden('RC\nGG').plants('Alice')).toEqual([
      'radishes',
      'clover',
      'grass',
      'grass',
    ])
  })

  it('different garden with single student', () => {
    expect(new Garden('VC\nRC').plants('Alice')).toEqual([
      'violets',
      'clover',
      'radishes',
      'clover',
    ])
  })

  it('garden with two students', () => {
    expect(new Garden('VVCG\nVVRC').plants('Bob')).toEqual([
      'clover',
      'grass',
      'radishes',
      'clover',
    ])
  })

  describe('multiple students for the same garden with three students', () => {
    it("second student's garden", () => {
      expect(new Garden('VVCCGG\nVVCCGG').plants('Bob')).toEqual([
        'clover',
        'clover',
        'clover',
        'clover',
      ])
    })

    it("third student's garden", () => {
      expect(new Garden('VVCCGG\nVVCCGG').plants('Charlie')).toEqual([
        'grass',
        'grass',
        'grass',
        'grass',
      ])
    })
  })
})

describe('full garden', () => {
  const diagram = 'VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV'
  const garden = new Garden(diagram)

  it("for Alice, first student's garden", () => {
    expect(garden.plants('Alice')).toEqual([
      'violets',
      'radishes',
      'violets',
      'radishes',
    ])
  })

  it("for Bob, second student's garden", () => {
    expect(garden.plants('Bob')).toEqual([
      'clover',
      'grass',
      'clover',
      'clover',
    ])
  })

  it('for Charlie', () => {
    expect(garden.plants('Charlie')).toEqual([
      'violets',
      'violets',
      'clover',
      'grass',
    ])
  })

  it('for David', () => {
    expect(garden.plants('David')).toEqual([
      'radishes',
      'violets',
      'clover',
      'radishes',
    ])
  })

  it('for Eve', () => {
    expect(garden.plants('Eve')).toEqual([
      'clover',
      'grass',
      'radishes',
      'grass',
    ])
  })

  it('for Fred', () => {
    expect(garden.plants('Fred')).toEqual([
      'grass',
      'clover',
      'violets',
      'clover',
    ])
  })

  it('for Ginny', () => {
    expect(garden.plants('Ginny')).toEqual([
      'clover',
      'grass',
      'grass',
      'clover',
    ])
  })

  it('for Harriet', () => {
    expect(garden.plants('Harriet')).toEqual([
      'violets',
      'radishes',
      'radishes',
      'violets',
    ])
  })

  it('for Ileana', () => {
    expect(garden.plants('Ileana')).toEqual([
      'grass',
      'clover',
      'violets',
      'clover',
    ])
  })

  it('for Joseph', () => {
    expect(garden.plants('Joseph')).toEqual([
      'violets',
      'clover',
      'violets',
      'grass',
    ])
  })

  it("for Kincaid, second to last student's garden", () => {
    expect(garden.plants('Kincaid')).toEqual([
      'grass',
      'clover',
      'clover',
      'grass',
    ])
  })

  it("for Larry, last student's garden", () => {
    expect(garden.plants('Larry')).toEqual([
      'grass',
      'violets',
      'clover',
      'violets',
    ])
  })
})

describe('disordered class', () => {
  const diagram = 'VCRRGVRG\nRVGCCGCV'
  const students = ['Samantha', 'Patricia', 'Xander', 'Roger']
  const garden = new Garden(diagram, students)

  it('for Patricia', () => {
    expect(garden.plants('Patricia')).toEqual([
      'violets',
      'clover',
      'radishes',
      'violets',
    ])
  })

  it('for Roger', () => {
    expect(garden.plants('Roger')).toEqual([
      'radishes',
      'radishes',
      'grass',
      'clover',
    ])
  })

  it('for Samantha', () => {
    expect(garden.plants('Samantha')).toEqual([
      'grass',
      'violets',
      'clover',
      'grass',
    ])
  })

  it('for Xander', () => {
    expect(garden.plants('Xander')).toEqual([
      'radishes',
      'grass',
      'clover',
      'violets',
    ])
  })
})

describe('Two gardens, different students', () => {
  const diagram = 'VCRRGVRG\nRVGCCGCV'
  const garden1 = new Garden(diagram, ['Alice', 'Bob', 'Charlie', 'Dan'])
  const garden2 = new Garden(diagram, ['Bob', 'Charlie', 'Dan', 'Erin'])

  it('Bob and Charlie for each garden', () => {
    expect(garden1.plants('Bob')).toEqual([
      'radishes',
      'radishes',
      'grass',
      'clover',
    ])
    expect(garden2.plants('Bob')).toEqual([
      'violets',
      'clover',
      'radishes',
      'violets',
    ])
    expect(garden1.plants('Charlie')).toEqual([
      'grass',
      'violets',
      'clover',
      'grass',
    ])
    expect(garden2.plants('Charlie')).toEqual([
      'radishes',
      'radishes',
      'grass',
      'clover',
    ])
  })
})
