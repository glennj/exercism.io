module pascals_triangle;

pure int[][] rows(int count) {
    int[][] triangle;
    for (int i = 0; i < count; i++) {
        auto row = new int[i + 1];
        row[0] = row[i] = 1;
        for (int j = 1; j < i; j++)
            row[j] = triangle[i-1][j-1] + triangle[i-1][j];
        triangle ~= row;
    }
    return triangle;
}

unittest
{
    immutable int allTestsEnabled = 1;

    // Zero rows
    {
        int[][] expected = [
        ];
        assert(rows(0) == expected);
    }

    static if (allTestsEnabled)
    {
        // Single row
        {
            int[][] expected = [
                [1],
            ];
            assert(rows(1) == expected);
        }

        // Two rows
        {
            int[][] expected = [
                [1],
                [1, 1],
            ];
            assert(rows(2) == expected);
        }

        // Three rows
        {
            int[][] expected = [
                [1],
                [1, 1],
                [1, 2, 1],
            ];
            assert(rows(3) == expected);
        }

        // Four rows
        {
            int[][] expected = [
                [1],
                [1, 1],
                [1, 2, 1],
                [1, 3, 3, 1],
            ];
            assert(rows(4) == expected);
        }

        // Five rows
        {
            int[][] expected = [
                [1],
                [1, 1],
                [1, 2, 1],
                [1, 3, 3, 1],
                [1, 4, 6, 4, 1],
            ];
            assert(rows(5) == expected);
        }

        // Six rows
        {
            int[][] expected = [
                [1],
                [1, 1],
                [1, 2, 1],
                [1, 3, 3, 1],
                [1, 4, 6, 4, 1],
                [1, 5, 10, 10, 5, 1],
            ];
            assert(rows(6) == expected);
        }

        // Ten rows
        {
            int[][] expected = [
                [1],
                [1, 1],
                [1, 2, 1],
                [1, 3, 3, 1],
                [1, 4, 6, 4, 1],
                [1, 5, 10, 10, 5, 1],
                [1, 6, 15, 20, 15, 6, 1],
                [1, 7, 21, 35, 35, 21, 7, 1],
                [1, 8, 28, 56, 70, 56, 28, 8, 1],
                [1, 9, 36, 84, 126, 126, 84, 36, 9, 1],
            ];
            assert(rows(10) == expected);
        }
    }
}
