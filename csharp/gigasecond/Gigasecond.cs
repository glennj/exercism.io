using System;

public static class Gigasecond
{
    // 1 billion seconds.
    private static readonly TimeSpan GigaSecond = new TimeSpan(0, 0, (int)1e9);

    public static DateTime Add(DateTime moment)
    {
        return moment + GigaSecond;
    }
}