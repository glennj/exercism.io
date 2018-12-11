function rand (n: number): number { return Math.floor(Math.random() * n) }
// random A-Z
function rA (): string { return String.fromCharCode(65 + rand(26)) }
// random 0-9
function rD (): string { return String(rand(10)) }

class RobotName {
  private static registry: Set<string> = new Set<string>()

  private _name: string
  get name(): string { return this._name }

  constructor() {
    this._name = this.generateName()
  }

  private generateName(): string {
    let name
    do {
      name = rA() + rA() + rD() + rD() + rD()
    } while (RobotName.registry.has(name))
    RobotName.registry.add(name)
    return name
  }

  resetName(): void {
    const newName = this.generateName()
    // do not reuse discarded names:
    // RobotName.registry.delete( this.name )
    RobotName.registry.add( newName )
    this._name = newName
  }
}

export default RobotName
