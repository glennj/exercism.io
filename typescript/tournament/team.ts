export class Team {
  private _name = ''
  private _wins = 0
  private _losses = 0
  private _draws = 0

  constructor(name: string) {
    this._name = name
  }

  win():  void { this._wins++ }
  lose(): void { this._losses++ }
  draw(): void { this._draws++ }

  get name():   string { return this._name }
  get wins():   number { return this._wins }
  get losses(): number { return this._losses }
  get draws():  number { return this._draws }

  get games():  number { return this.wins + this.losses + this.draws }
  get points(): number { return 3 * this.wins + this.draws }

  cmp(other: Team): number {
    if (this.points > other.points) return -1   // this sorts before other
    if (this.points < other.points) return 1
    if (this.name < other.name) return -1
    if (this.name > other.name) return 1
    return 0
  }
}
