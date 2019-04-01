using System;
using System.Collections.Generic;

public class SpaceAge
{
    private static readonly int SecondsPerEarthYear = 31557600;

    private static readonly Dictionary<string, double> OrbitalPeriod = new Dictionary<string, double>() {
        {"Mercury", 0.2408467},
        {"Venus",   0.61519726},
        {"Earth",   1.0},
        {"Mars",    1.8808158},
        {"Jupiter", 11.862615},
        {"Saturn",  29.447498},
        {"Uranus",  84.016846},
        {"Neptune", 164.79132},
    };

    private int age;

    public SpaceAge(int seconds)
    {
        this.age = seconds;
    }

    private double OnPlanet(string planet)
    {
        return (double)this.age
                / SecondsPerEarthYear
                / OrbitalPeriod[planet];
    }

    public double OnEarth()
    {
        return this.OnPlanet("Earth");
    }

    public double OnMercury()
    {
        return this.OnPlanet("Mercury");
    }

    public double OnVenus()
    {
        return this.OnPlanet("Venus");
    }

    public double OnMars()
    {
        return this.OnPlanet("Mars");
    }

    public double OnJupiter()
    {
        return this.OnPlanet("Jupiter");
    }

    public double OnSaturn()
    {
        return this.OnPlanet("Saturn");
    }

    public double OnUranus()
    {
        return this.OnPlanet("Uranus");
    }

    public double OnNeptune()
    {
        return this.OnPlanet("Neptune");
    }
}
