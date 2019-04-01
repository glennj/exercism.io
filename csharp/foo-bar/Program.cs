using System;

namespace foo_bar
{
    class Program
    {
        private static Random Rnd = new Random();

        private static string RandChar()
        {
            return "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[Rnd.Next(26)].ToString();
        }
        private static string RandDigit()
        {
            return "0123456789"[Rnd.Next(10)].ToString();
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
            string n = RandChar() + RandChar()
                        + RandDigit() + RandDigit() + RandDigit();
            Console.WriteLine(n);

        }
    }
}
