using System;
using System.Collections.Generic;

[FlagsAttribute]
public enum Allergen
{
    Eggs = 1,
    Peanuts = 2,
    Shellfish = 4,
    Strawberries = 8,
    Tomatoes = 16,
    Chocolate = 32,
    Pollen = 64,
    Cats = 128
}

public class Allergies
{
    private Allergen Mask;

    public Allergies(int mask)
    {
        this.Mask = (Allergen)mask;
    }

    public bool IsAllergicTo(Allergen allergen)
    {
        return this.Mask.HasFlag(allergen);
    }

    public Allergen[] List()
    {
        var allergens = new List<Allergen>();
        foreach (Allergen allergen in Enum.GetValues(typeof(Allergen)))
        {
            if (this.IsAllergicTo(allergen)) 
            {
                allergens.Add(allergen);
            }
        }
        return allergens.ToArray();
    }
}