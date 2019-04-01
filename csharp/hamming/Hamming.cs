using System;
using System.Linq;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        firstStrand = firstStrand ?? "";
        secondStrand = secondStrand ?? "";

        if (firstStrand.Length != secondStrand.Length)
            throw new ArgumentException();

        /*
        int distance = 0;
        for (var i = 0; i < firstStrand.Length; i++)
            if (firstStrand[i] != secondStrand[i])
                distance++;

        return distance;
        */

        return firstStrand
                .Where((ch, idx) => ch != secondStrand[idx])
                .Count();
    }
}