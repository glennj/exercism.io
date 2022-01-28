
// generate all names
const ALL_NAMES: string[] = []
let currentIndex: number

for (let a = 65; a <= 90; a++) {
  const A = String.fromCharCode(a)

  for (let b = 65; b <= 90; b++) {
    const B = String.fromCharCode(b)

    for (let c = 0; c <= 999; c++) {
      ALL_NAMES.push(A + B + String(c).padStart(3, '0'))
    }
  }
}

function shuffle(): void {
  for (let i = ALL_NAMES.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [ALL_NAMES[i], ALL_NAMES[j]] = [ALL_NAMES[j], ALL_NAMES[i]];
  }
}

export class Robot {
  static releaseNames(): void {
    currentIndex = 0
    shuffle()
  }

  private myName!: string

  constructor() {
    this.resetName()
  }

  get name(): string {
    return this.myName
  }

  resetName(): void {
    if (currentIndex == ALL_NAMES.length)
      throw new Error('All names in use')
    this.myName = ALL_NAMES[currentIndex++]
  }
}

Robot.releaseNames()
