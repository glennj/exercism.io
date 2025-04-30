Number.prototype.padStart = function (targetLength, padString = " ") {
  return this.toString().padStart(targetLength, padString);
};

const HEADER = "Team                           | MP |  W |  D |  L |  P";

function formatTeam(team) {
  return (
    team.name.padEnd(30) +
    " | " +
    team.matchesPlayed.padStart(2) +
    " | " +
    team.wins.padStart(2) +
    " | " +
    team.draws.padStart(2) +
    " | " +
    team.losses.padStart(2) +
    " | " +
    team.points.padStart(2)
  );
}

function addTeam(teams, teamName) {
  if (!Object.hasOwn(teams, teamName)) {
    teams[teamName] = { wins: 0, losses: 0, draws: 0 };
  }
}

function processResult(teams, home, away, result) {
  addTeam(teams, home);
  addTeam(teams, away);

  if (result === "win") {
    teams[home].wins++;
    teams[away].losses++;
  } else if (result === "loss") {
    teams[home].losses++;
    teams[away].wins++;
  } else {
    teams[home].draws++;
    teams[away].draws++;
  }
}

function buildStandings(teams) {
  const teamList = [];
  for (const teamName in teams) {
    const t = teams[teamName];
    t.name = teamName;
    t.points = t.wins * 3 + t.draws;
    t.matchesPlayed = t.wins + t.losses + t.draws;
    teamList.push(t);
  }

  const standings = [HEADER];
  for (const team of sort(teamList, "points", "name")) {
    standings.push(formatTeam(team));
  }

  return standings;
}

export function tournamentTally(matchResults) {
  const teams = {};

  for (const match of matchResults.split("\n")) {
    let [home, away, result] = match.split(";");
    if (home && away && result) {
      processResult(teams, home, away, result);
    }
  }

  return buildStandings(teams).join("\n");
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
