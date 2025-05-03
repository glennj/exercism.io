// monkey patching! necessary?
Number.prototype.padStart = function (targetLength, padString = " ") {
  return this.toString().padStart(targetLength, padString);
};

// -----------------------------------------------------------
function processResults(matchResults) {
  const teams = {};
  for (const match of matchResults.split("\n")) {
    let [home, away, result] = match.split(";");
    if (!(home && away && result)) continue;
    processMatch(teams, home, away, result);
  }
  return Object.values(teams);
}

function processMatch(teams, home, away, result) {
  registerTeam(teams, home);
  registerTeam(teams, away);

  if (result === "win") {
    win(teams[home]);
    lose(teams[away]);
  } else if (result === "loss") {
    win(teams[away]);
    lose(teams[home]);
  } else {
    draw(teams[home]);
    draw(teams[away]);
  }
}

function win(team) {
  team.wins++;
  team.points += 3;
  team.matchesPlayed++;
}
function lose(team) {
  team.losses++;
  team.matchesPlayed++;
}
function draw(team) {
  team.draws++;
  team.points += 1;
  team.matchesPlayed++;
}

function registerTeam(teams, teamName) {
  if (Object.hasOwn(teams, teamName)) return;
  // prettier-ignore
  teams[teamName] = {name: teamName, wins: 0, draws: 0, losses: 0, matchesPlayed: 0,points: 0};
}

function buildStandings(teams) {
  const standings = sort(teams, "points", "name").map(formatTeam);
  standings.unshift(header());
  return standings.join("\n");
}

const header = () =>
  // prettier-ignore
  formatTeam({name: "Team", matchesPlayed: "MP", wins: "W", losses: "L", draws: "D", points: "P"});

function formatTeam(team) {
  return [
    team.name.padEnd(30),
    team.matchesPlayed.padStart(2),
    team.wins.padStart(2),
    team.draws.padStart(2),
    team.losses.padStart(2),
    team.points.padStart(2),
  ].join(" | ");
}

// -----------------------------------------------------------
export function tournamentTally(matchResults) {
  const teams = processResults(matchResults);
  return buildStandings(teams);
}

// We've provided you this function. You might be interested to
// explore how it works, but you don't need to understand it
// or change it. Read the instructions to see how to use it!
function sort(data, pointsKey, nameKey) {
  return data.sort((a, b) => {
    const pointsComparison = b[pointsKey] - a[pointsKey];
    if (pointsComparison != 0) {
      return pointsComparison;
    }
    return a[nameKey].localeCompare(b[nameKey]);
  });
}
