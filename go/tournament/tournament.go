package tournament

import (
	"errors"
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

	for _, line := range strings.Split(string(bytes), "\n") {
		line = strings.TrimSpace(line)

		// ignore blank lines and comments
		if line == "" || line[0] == '#' { continue }

		fields := strings.Split(line, ";")
		if len(fields) != 3 {
			return nil, errors.New(fmt.Sprintf("line must have 3 fields '%s'", line))
		}

		home := teams.GetTeam(fields[0])
		away := teams.GetTeam(fields[1])

		switch fields[2] {
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
			return nil, errors.New(fmt.Sprintf("invalid result in line '%s'", line))
		}
	}
	return teams, nil
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
