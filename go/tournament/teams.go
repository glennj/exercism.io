package tournament

type Teams []*Team

func (t *Teams) GetTeam(name string) *Team {
	for _, team := range *t {
		if team.Name == name {
			return team
		}
	}

	// else, add a new one
	team := &Team{Name: name}
	*t = append(*t, team)
	return team
}

// sort interface

func (t Teams) Len() int      { return len(t) }
func (t Teams) Swap(i, j int) { t[i], t[j] = t[j], t[i] }
func (t Teams) Less(i, j int) bool {
	return t[i].Points > t[j].Points ||
		(t[i].Points == t[j].Points && t[i].Name < t[j].Name)
}
