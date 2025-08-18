public class EliudsEggs {
    public int eggCount(int number) {
        /*
        return eggCountIterative(number);
        */
        return eggCountRecursive(number, 0);
    }

    private int eggCountIterative(int number) {
        int count = 0;
        while (number > 0) {
            count += number & 1;
            number >>= 1;
        }
        return count;
    }

    private int eggCountRecursive(int number, int count) {
        if (number == 0)
            return count;
        else
            return eggCountRecursive(
                    number >> 1,
                    count + (number & 1)
            );
    }
}
