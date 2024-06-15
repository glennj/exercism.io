import {Team} from './team'

const TABLE_HEADER = 'Team                           | MP |  W |  D |  L |  P'

export class Tournament {
  private teams = new Map<string, Team>()

  public tally(input: string): string {
    this.processResults(input)
    return this.standings()
  }

  private processResults(input: string): void {
    for (const line of input.split(/\n/)) {
      const fields = line.split(/;/)
      // skip ill-formed lines
      if (fields.length !== 3) continue

      const [home, away, result] = fields

      if (!this.teams.has(home)) {
        this.teams.set(home, new Team(home))
      }
      if (!this.teams.has(away)) {
        this.teams.set(away, new Team(away))
      }

      switch (result) {
        case 'win':
          this.teams.get(home)?.win()
          this.teams.get(away)?.lose()
          break
        case 'loss':
          this.teams.get(home)?.lose()
          this.teams.get(away)?.win()
          break
        case 'draw':
          this.teams.get(home)?.draw()
          this.teams.get(away)?.draw()
          break
      }
    }
  }

  private standings(): string {
    const teams = []
    for (const team of this.teams.values()) {
      teams.push(team)
    }

    teams.sort((a, b) => a.cmp(b))

    return [TABLE_HEADER]
           .concat(teams.map(this.teamInfo))
           .join('\n')
  }

  private teamInfo(team: Team): string {
    return [team.name.padEnd(30)]
      .concat(
        [team.games, team.wins, team.draws, team.losses, team.points]
        .map(n => n.toString().padStart(2))
      )
      .join(' | ')
  }
}
