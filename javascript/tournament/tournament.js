const STATS = ['MP', 'W', 'D', 'L', 'P']; Object.freeze(STATS);

const tableRow = (name, statistics) => [
    name.padEnd(30),
    ...statistics.map(s => String(s).padStart(2))
  ].join(' | ');

const header = () => tableRow('Team', STATS);

/* *********************************************************** */
class Team {
  constructor(name) {
    this.teamName = name;
    this.stats = STATS.reduce((o,s) => {o[s]=0; return o}, {});

  }

  name()   { return this.teamName; }
  points() { return this.stats.P; }

  win()  { this.stats.MP++; this.stats.W++; this.stats.P += 3; }
  draw() { this.stats.MP++; this.stats.D++; this.stats.P += 1; }
  lose() { this.stats.MP++; this.stats.L++; }

  tableEntry() {
    return tableRow(this.teamName, STATS.map(s => this.stats[s]));
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

  // The consecutive sort calls assume the javascript version
  // implements a stable sort.
  tally() {
    const table = Object.values(this.teams)
      .sort((a, b) => a.name().localeCompare(b.name()))  // ascending
      .sort((a, b) => -(a.points() - b.points()))        // descending
      .map(team => team.tableEntry());
    table.unshift(header());
    return table.join("\n");
  }
}

/* *********************************************************** */
export const tournamentTally = (input) => {
  return new Tournament().parse(input).tally();
};
