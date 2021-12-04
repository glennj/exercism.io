const HEADER = 'Team                           | MP |  W |  D |  L |  P';

/* *********************************************************** */
class Team {
  constructor(name) {
    this.teamName = name;
    this.w = 0;
    this.d = 0;
    this.l = 0;
    this.mp = 0;
    this.p = 0;
  }

  name()   { return this.teamName; }
  points() { return this.p; }

  win()  { this.w++; this.mp++; this.p += 3; }
  draw() { this.d++; this.mp++; this.p += 1; }
  lose() { this.l++; this.mp++; }

  tableEntry() {
    return this.teamName.padEnd(30)    + ' | ' +
           String(this.mp).padStart(2) + ' | ' +
           String(this.w).padStart(2)  + ' | ' +
           String(this.d).padStart(2)  + ' | ' +
           String(this.l).padStart(2)  + ' | ' +
           String(this.p).padStart(2);
  }
}

/* *********************************************************** */
class Tournament {
  constructor() {
    this.teams = {};
  }

  parse(input) {
    const teams = this.teams;
    input.split("\n").forEach(match => {
      const [home, away, result] = match.split(";");
      if (home && away && result) {
        if (!(home in teams)) teams[home] = new Team(home);
        if (!(away in teams)) teams[away] = new Team(away);

        if (result === 'win') {
          teams[home].win();
          teams[away].lose();
        }
        if (result === 'draw') {
          teams[home].draw();
          teams[away].draw();
        }
        if (result === 'loss') {
          teams[home].lose();
          teams[away].win();
        }
      }
    });
    return this;
  }

  tally() {
    const table = Object.values(this.teams)
      .sort((a, b) => a.name() < b.name() ? -1 : (a.name() > b.name() ? 1 : 0))
      .sort((a, b) => b.points() - a.points())
      .map(team => team.tableEntry());
    return [HEADER].concat(table).join("\n");
  }
}

/* *********************************************************** */
export const tournamentTally = (input) => {
  return new Tournament().parse(input).tally();
};
