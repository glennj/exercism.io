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
        int otherPoints = other.getPoints();
        if (myPoints > otherPoints)
            return -1;
        if (myPoints < otherPoints)
            return 1;
        return getName().compareTo(other.getName());
    }
}
