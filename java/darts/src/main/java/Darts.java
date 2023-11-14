class Darts {
    private static double OUTER_RADIUS  = 10.0;
    private static double MIDDLE_RADIUS =  5.0;
    private static double INNER_RADIUS  =  1.0;
    private static int OUTER_SCORE  =  1;
    private static int MIDDLE_SCORE =  5;
    private static int INNER_SCORE  = 10;

    int score(double x, double y) {
        double distance = Math.sqrt(x * x + y * y);
        int score = 0;
        if (distance <= INNER_RADIUS) {
            score = INNER_SCORE;
        }
        else if (distance <= MIDDLE_RADIUS) {
            score = MIDDLE_SCORE;
        }
        else if (distance <= OUTER_RADIUS) {
            score = OUTER_SCORE;
        }
        return score;
    }
}
