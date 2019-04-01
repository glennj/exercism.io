using System;
using System.Collections.Generic;

public class Robot
{
    private static readonly HashSet<string> Names = new HashSet<string>();
    private static readonly Random Rnd = new Random();

    public Robot()
    {
        this.GenerateNewName();
    }

    public string Name
    { get; private set; }

    public void Reset()
    {
        this.GenerateNewName();
    }

    private void GenerateNewName()
    {
        string name;
        do
        {
            name = RandUpper() + RandUpper() +
                   RandDigit() + RandDigit() + RandDigit();
        } while (Robot.Names.Contains(name));

        Robot.Names.Add(name);
        this.Name = name;
    }

    private string RandUpper()
    {
        return RandChar("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    }
    
    private string RandDigit()
    {
        return RandChar("0123456879");
    }

    private string RandChar(string alphabet)
    {
        return alphabet[Robot.Rnd.Next(alphabet.Length)].ToString();
    }
}
