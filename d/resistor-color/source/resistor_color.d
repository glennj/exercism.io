module resistor_color;

struct ResistorColor {
    static immutable auto colors = [
        "black", "brown", "red", "orange", "yellow",
        "green", "blue", "violet", "grey", "white",
    ];

    static pure short colorCode(immutable string color) {
        foreach (i, c; colors)
            if (c == color)
                return cast(short) i;
        return -1;
    }
}


unittest
{
    immutable int allTestsEnabled = 1;

    // Black
    assert(ResistorColor.colorCode("black") == 0);

    static if (allTestsEnabled)
    {
        // White
        assert(ResistorColor.colorCode("white") == 9);

        // Orange
        assert(ResistorColor.colorCode("orange") == 3);

        // Colors
        assert(ResistorColor.colors == [
                "black", "brown", "red", "orange", "yellow", "green", "blue",
                "violet", "grey", "white"
                ]);
    }
}
