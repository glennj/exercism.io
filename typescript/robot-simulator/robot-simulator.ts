// all dispatched methods must have this signature.
type DispatchFunction = () => void

class Robot {

  // Using 'keyof Robot' type to enable dispatching.
  // See `instructions` and `evaluate` methods.
  private static instructionSet: {[key: string]: keyof Robot} = {
    A: 'advance',
    R: 'turnRight',
    L: 'turnLeft',
  }

  // Using specific order. See `advance()` method.
  private static bearings: string[] = ['east', 'north', 'west', 'south']

  private _coordinates: [number, number]
  private _bearing: number

  constructor(x: number = 0, y: number = 0, bearing: string = 'north') {
    this.at(x, y)
    this.orient(bearing)
  }

  get bearing()     { return Robot.bearings[this._bearing] }
  get coordinates() { return this._coordinates }

  at(x: number, y: number): void {
    this._coordinates = [x, y]
  }

  orient(bearing: string): void {
    this._bearing = Robot.bearings.indexOf(bearing)
    if (this._bearing === -1) {
      throw new Error('Invalid Robot Bearing')
    }
  }

  turn(direction: number): void {
    this._bearing = (this._bearing + direction + 4) % 4
  }

  turnLeft():  void { this.turn(+1) }     // DispatchFunction signature
  turnRight(): void { this.turn(-1) }     // DispatchFunction signature
  advance(): void {                       // DispatchFunction signature
    const rad = this._bearing * 90 * 2 * Math.PI / 360
    const [x, y] = this._coordinates
    const [dx, dy] = [Math.round(Math.cos(rad)), Math.round(Math.sin(rad))]
    this.at(x + dx, y + dy)
  }

  instructions(script: string): Array<keyof Robot> {
    return [...script].map((instr) => {
      if (!Robot.instructionSet[instr]) {
        throw new Error('Invalid instruction')
      }
      return Robot.instructionSet[instr]
    })
  }

  evaluate(script: string): void {
    this.instructions(script).forEach((instruction) => {
      ( this[instruction] as DispatchFunction )()
    })
  }
}

export default Robot
