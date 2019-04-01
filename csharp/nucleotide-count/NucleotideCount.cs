using System;
using System.Collections.Generic;

public static class NucleotideCount
{
    public static IDictionary<char, int> Count(string sequence)
    {
        var count = new Dictionary<char, int>
        {
            ['G'] = 0,
            ['C'] = 0,
            ['A'] = 0,
            ['T'] = 0,
        };

        foreach (var ch in sequence)
        {
            if (!count.ContainsKey(ch)) 
            {
                throw new ArgumentException();
            }
            count[ch]++;
        }
        return count;
    }
}

/* https://exercism.io/tracks/csharp/exercises/nucleotide-count/solutions/a725a2119eaa4440b8ae1da8145131f2

    public static IDictionary<char, int> Count(string sequence)
    {
        if (!sequence.All("ACGT".Contains))
            throw new ArgumentException();

        return (sequence + "ACGT")
                .GroupBy(x => x)
                .ToDictionary(g => g.Key, g => g.Count() - 1);
    }
 */