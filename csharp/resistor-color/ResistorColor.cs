using System;

public static class ResistorColor
{
    private static readonly string[] COLORS = {
        "black", "brown", "red", "orange", "yellow",
        "green", "blue", "violet", "grey", "white",
    };

    public static int ColorCode(string color)
    {
        int index = Array.IndexOf(COLORS, color);
        // TODO color not found ? Requirement?
        return index;
    }

    public static string[] Colors()
    {
        /* https://exercism.io/tracks/csharp/exercises/resistor-color/solutions/508edcc6d4754e0ab9d65b96f2390c97#comment-%23%3CSolutionComment:0x00007f1d4cddd748%3E
         * 
         * me: question: the array is readonly, is it necessary to clone?
         * NobbZ: Yes, only the reference to the array is read-only, not the contents though.
         */
        return (string[]) COLORS.Clone();
    }
}