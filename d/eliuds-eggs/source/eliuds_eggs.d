module eliuds_eggs;

pure int eggCount(immutable int number) {
    int count = 0, n = number;
    while (n > 0) {
        count += n & 1;
        n >>= 1;
    }
    return count;
}

unittest
{
    immutable int allTestsEnabled = 1;

    // 0 eggs
    assert(eggCount(0) == 0);

    static if (allTestsEnabled)
    {
        // 1 egg
        assert(eggCount(16) == 1);

        // 4 eggs
        assert(eggCount(89) == 4);

        // 13 eggs
        assert(eggCount(2_000_000_000) == 13);
    }

}
