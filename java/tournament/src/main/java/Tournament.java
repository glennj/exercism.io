import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

public class Tournament {
    private Map<String, Team> teams = new HashMap<>();

    void applyResults(String results) {
        for (String match : results.split("\n")) {
            String[] items = match.split(";");
            if (items.length != 3)
                throw new IllegalArgumentException("Invalid match result");

            Team home = teams.computeIfAbsent(items[0], Team::new);
            Team away = teams.computeIfAbsent(items[1], Team::new);

            switch (items[2]) {
                case "won":
                    home.won();
                    away.lost();
                    break;
                case "lost":
                    home.lost();
                    away.won();
                    break;
                case "drew":
                    home.drew();
                    away.drew();
                    break;
                default:
                    throw new IllegalArgumentException("Invalid match result");
            }
        }
    }

    String printTable() {
        StringBuilder sb = new StringBuilder();
        sb.append("Team                           | MP |  W |  D |  L |  P\n");
        String matchFormat = "%-30s | %2d | %2d | %2d | %2d | %2d\n";

        List<Team> teamList = new ArrayList<>(teams.values());
        Collections.sort(teamList);
        for (Team t : teamList) {
            sb.append(String.format(matchFormat,
                    t.getName(),
                    t.getMatchesPlayed(),
                    t.getWins(),
                    t.getDraws(),
                    t.getLosses(),
                    t.getPoints()
            ));
        }
        return sb.toString();
    }
}
