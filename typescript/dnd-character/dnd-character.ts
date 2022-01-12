/* a DnD Character.
 * Note: using a class static method in a constructor or
 * instance method is awkward. I haven't found a way not to
 * hardcode the class name. I've put the functionality into
 * the module body unexported, and the DnDCharacter
 * static methods and instance creation use those.
 */


// roll an n-sided die
const d = (faces: number): number => 1 + Math.floor(Math.random() * faces)

const ability = (): number => {
  const rolls = [d(6), d(6), d(6), d(6)]
  return rolls.reduce((sum, n) => sum + n) - Math.min(...rolls)
}

const modifier = (n: number): number => Math.floor((n - 10) / 2)

export class DnDCharacter {
  public static generateAbilityScore = (): number => ability()

  public static getModifierFor = (n: number): number => modifier(n)

  // instance fields
  readonly strength     : number = ability()
  readonly dexterity    : number = ability()
  readonly constitution : number = ability()
  readonly intelligence : number = ability()
  readonly wisdom       : number = ability()
  readonly charisma     : number = ability()
  readonly hitpoints    : number = ability()

  constructor() {
    this.hitpoints = 10 + modifier(this.constitution)
  }
}
