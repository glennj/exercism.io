class Allergies {
  private readonly allergies: string[] = []

  constructor(score: number) {
    for (const [value, allergen] of Allergies.allergens) {
      if (score & value) {
        this.allergies.push(allergen)
      }
    }
  }

  static allergens: Map<number, string> = new Map([
    [1, 'eggs'],
    [2, 'peanuts'],
    [4, 'shellfish'],
    [8, 'strawberries'],
    [16, 'tomatoes'],
    [32, 'chocolate'],
    [64, 'pollen'],
    [128, 'cats'],
  ])

  list(): string[] { return this.allergies }

  allergicTo(allergen: string): boolean {
    return this.allergies.includes(allergen)
  }
}

export { Allergies }
