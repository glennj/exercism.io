package tournament

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

func Tally(reader io.Reader, writer io.Writer) error {
	teams, err := processInput(reader)
	if err != nil {
		return fmt.Errorf("could not process the input: %w", err)
	}

	sort.Sort(teams)

	return emitOutput(writer, teams)
}

// ---------------------------------------------------------
func processInput(reader io.Reader) (Teams, error) {
	bytes, err := io.ReadAll(reader)
	if err != nil {
		return nil, fmt.Errorf("error reading input: %w", err)
	}

	teams := Teams{}
	/* Teams is a slice of Team pointers.
	 * The zero value, Teams{} is a empty non-nil slice.
	 * However, nil is the zero value for slices, so it works too.
	 */

	for _, line := range strings.Split(string(bytes), "\n") {
		line = strings.TrimSpace(line)

		// ignore blank lines and comments
		if line == "" || line[0] == '#' {
			continue
		}

		fields := strings.Split(line, ";")
		if len(fields) != 3 {
			return nil, fmt.Errorf("line must have 3 fields '%s'", line)
		}

		err := registerMatch(teams.GetTeam(fields[0]), teams.GetTeam(fields[1]), fields[2])
		if err != nil {
			return nil, fmt.Errorf("cannot process line '%s': %w", line, err)
		}

	}
	return teams, nil
}

func registerMatch(home, away *Team, result string) error {
	switch result {
	case "win":
		home.Win()
		away.Lose()
	case "loss":
		home.Lose()
		away.Win()
	case "draw":
		home.Draw()
		away.Draw()
	default:
		return fmt.Errorf("invalid result '%s'", result)
	}
	return nil
}

// ---------------------------------------------------------
func emitOutput(writer io.Writer, teams Teams) error {
	f := "%-30v | %2v | %2v | %2v | %2v | %2v\n"

	_, err := io.WriteString(writer, fmt.Sprintf(f, "Team", "MP", "W", "D", "L", "P"))
	if err != nil {
		return fmt.Errorf("cannot write header: %w", err)
	}

	for _, t := range teams {
		row := fmt.Sprintf(f, t.Name, t.Matches, t.Wins, t.Draws, t.Losses, t.Points)
		_, err := io.WriteString(writer, row)
		if err != nil {
			return fmt.Errorf("cannot write row '%s': %w", row, err)
		}
	}
	return nil
}
