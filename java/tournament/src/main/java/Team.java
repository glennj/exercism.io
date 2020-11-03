public class Team implements Comparable<Team> {
    private int wins = 0;
    private int draws = 0;
    private int losses = 0;
    private final String name;

    Team(String name) {
        this.name = name;
    }

    String getName()       { return name; }
    int getWins()          { return wins; }
    int getDraws()         { return draws; }
    int getLosses()        { return losses; }

    int getMatchesPlayed() { return wins + draws + losses; }
    int getPoints()        { return 3 * wins + draws; }

    void won()  { wins++; }
    void drew() { draws++; }
    void lost() { losses++; }

    /**
     * sort first by points (descending), then by name (ascending)
     */
    public int compareTo(Team other) {
        int myPoints = getPoints();
        int cmp = -1 * Integer.compare(myPoints, other.getPoints());
        return (cmp != 0) ? cmp : getName().compareTo(other.getName());
    }
}
