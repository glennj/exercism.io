import java.math.BigInteger;

class TwoBucket {

    TwoBucket(int size1, int size2, int goal, String startName) {
        validate(size1, size2, goal, startName);

        Bucket one = new Bucket("one", size1);
        Bucket two = new Bucket("two", size2);

        if (startName.equals(one.getName()))
            solve(one, two, goal);
        else
            solve(two, one, goal);
    }

    private int moves;
    private int otherAmount;
    private String finalBucket;

    int getTotalMoves() { return moves; }
    int getOtherBucket() { return otherAmount; }
    String getFinalBucket() { return finalBucket; }

    private void solve(Bucket start, Bucket other, int goal) {
        start.fill();
        moves = 1;
        while (true) {
            if (start.getUsed() == goal) {
                finalBucket = start.getName();
                otherAmount = other.getUsed();
                return;
            }

            if (other.getUsed() == goal) {
                finalBucket = other.getName();
                otherAmount = start.getUsed();
                return;
            }

            if (other.getSize() == goal)
                other.fill();
            else if (start.isEmpty())
                start.fill();
            else if (other.isFull())
                other.empty();
            else
                start.pour(other);

            moves++;
        }
    }

    private void validate(int size1, int size2, int goal, String startName) {
        if (goal > Math.max(size1, size2))
            throw new IllegalArgumentException("Cannot satisfy goal: too big");
        if (startName != "one" && startName != "two")
            throw new IllegalArgumentException("Bad start name");
        int gcd = gcd(size1, size2);
        if (gcd != 1 && (goal % gcd) != 0)
            throw new IllegalArgumentException("Cannot satisfy goal: not relatively prime");
    }

    private int gcd(int a, int b) {
        return b > 0 ? gcd(b, a % b) : a;
    }
}