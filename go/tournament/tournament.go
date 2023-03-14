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
		return err
	}

	sort.Sort(teams)

	return emitOutput(writer, teams)
}

// ---------------------------------------------------------
func processInput(reader io.Reader) (teams Teams, err error) {
	bytes, err := io.ReadAll(reader)
	if err != nil { return }

	for _, line := range strings.Split(string(bytes), "\n") {
		line = strings.TrimSpace(line)

		// ignore blank lines and comments
		if line == "" || line[0] == '#' { continue }

		fields := strings.Split(line, ";")
		if len(fields) != 3 {
			err = errors.New("must be 3 fields per line")
			return
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
			err = errors.New("invalid result")
			return
		}
	}
	return
}

// ---------------------------------------------------------
func emitOutput(writer io.Writer, teams Teams) (err error) {
	f := "%-30v | %2v | %2v | %2v | %2v | %2v\n"

	_, err = io.WriteString(writer, fmt.Sprintf(f, "Team", "MP", "W", "D", "L", "P"))
	if err != nil { return }

	for _, t := range teams {
		row := fmt.Sprintf(f, t.Name, t.Matches, t.Wins, t.Draws, t.Losses, t.Points)
		_, err = io.WriteString(writer, row)
		if err != nil { return }
	}
	return
}
