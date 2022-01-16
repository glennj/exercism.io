class Team:
    def __init__(self, name):
        self.name = name
        self.wins = 0
        self.losses = 0
        self.draws = 0

    def win(self):
        self.wins += 1

    def lose(self):
        self.losses += 1

    def draw(self):
        self.draws += 1

    def points(self):
        return 3 * self.wins + self.draws

    def played(self):
        return self.wins + self.losses + self.draws

    def standings_data(self):
        return [
            self.name,
            self.played(),
            self.wins,
            self.draws,
            self.losses,
            self.points()
        ]

    def __lt__(self, other):
        ''' for sorting:
        I am "less than" the other if:
          - I have more points, or
          - we have same points, but my name sorts first
        '''
        a = self.points()
        b = other.points()
        return a > b or (a == b and self.name < other.name)


def tally(tournament_results):
    return format_standings(parse_results(tournament_results))


def format_standings(teams):
    fmt = '{:<30} | {:>2} | {:>2} | {:>2} | {:>2} | {:>2}'
    return [fmt.format('Team', 'MP', 'W', 'D', 'L', 'P')] \
         + [fmt.format(*team.standings_data()) for team in sorted(teams)]


def parse_results(tournament_results):
    teams = {}

    for item in tournament_results:
        fields = item.split(";")
        if len(fields) != 3:
            continue
        home, away, result = fields

        if home not in teams:
            teams[home] = Team(home)
        if away not in teams:
            teams[away] = Team(away)

        if result == 'win':
            teams[home].win()
            teams[away].lose()
        elif result == 'loss':
            teams[home].lose()
            teams[away].win()
        elif result == 'draw':
            teams[home].draw()
            teams[away].draw()
        else:
            raise ValueError('Unknown result: ' + result)

    return teams.values()
