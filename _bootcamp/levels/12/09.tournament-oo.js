// monkey patching! necessary?
Number.prototype.padStart = function (targetLength, padString = " ") {
  return this.toString().padStart(targetLength, padString);
};

// -----------------------------------------------------------
// prettier-ignore
class Team {
  constructor(name) {
    this.name = name;
    this.wins = 0;
    this.draws = 0;
    this.losses = 0;
  }

  get points()        { return this.wins * 3 + this.draws }
  get matchesPlayed() { return this.wins + this.draws + this.losses }

  win()  { this.wins++ }
  draw() { this.draws++ }
  lose() { this.losses++ }
}

// -----------------------------------------------------------
class Tournament {
  constructor() {
    this.teams = {};
  }

  processResults(matchResults) {
    for (const match of matchResults.split("\n")) {
      const [home, away, result] = match.split(";");
      if (!(home && away && result)) continue;

      this._registerTeam(home);
      this._registerTeam(away);

      if (result === "win") {
        this.teams[home].win();
        this.teams[away].lose();
      } else if (result === "loss") {
        this.teams[home].lose();
        this.teams[away].win();
      } else if (result === "draw") {
        this.teams[home].draw();
        this.teams[away].draw();
      }
    }
    return this;
  }

  _registerTeam(teamName) {
    if (!Object.hasOwn(this.teams, teamName)) {
      this.teams[teamName] = new Team(teamName);
    }
  }

  buildStandings() {
    const sorted = Object.values(this.teams).sort((a, b) => {
      const pointsComparison = b.points - a.points;
      if (pointsComparison != 0) {
        return pointsComparison;
      }
      return a.name.localeCompare(b.name);
    });
    
    return [
      "Team                           | MP |  W |  D |  L |  P",
      ...sorted.map(this._formatForStandings),
    ].join("\n");
  }

  _formatForStandings(team) {
    return [
      team.name.padEnd(30),
      team.matchesPlayed.padStart(2),
      team.wins.padStart(2),
      team.draws.padStart(2),
      team.losses.padStart(2),
      team.points.padStart(2),
    ].join(" | ");
  }
}

// -----------------------------------------------------------
export const tournamentTally = (matchResults) =>
  new Tournament().processResults(matchResults).buildStandings();
