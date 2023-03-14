package tournament

type Team struct {
	Name    string
	Wins    int
	Losses  int
	Draws   int
	Points  int
	Matches int
}

func (t *Team) Win() {
	t.Wins++
	t.Matches++
	t.Points += 3
}

func (t *Team) Lose() {
	t.Losses++
	t.Matches++
}

func (t *Team) Draw() {
	t.Draws++
	t.Points++
	t.Matches++
}
