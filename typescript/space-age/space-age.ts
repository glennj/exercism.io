const SECONDS_PER_YEAR: number = 31557600   // seconds per Earth year

const REL_YEARS: {[key: string]: number} = {
  mercury:   0.2408467,
  venus:     0.61519726,
  earth:     1,
  mars:      1.8808158,
  jupiter:  11.862615,
  saturn:   29.447498,
  uranus:   84.016846,
  neptune: 164.79132,
}

export function age(planet: string, ageInSeconds: number): number {
  if (planet in REL_YEARS) {
    const age = ageInSeconds / SECONDS_PER_YEAR / REL_YEARS[planet]
    return Math.round(age * 100) / 100
  }

  throw new Error(`Not a planet: ${planet}`)
}



class SpaceAge {
  seconds: number

  constructor(seconds: number) {
    this.seconds = seconds
  }

  private onPlanet(planet: string): number {
    const age = this.seconds / SECONDS_PER_YEAR / REL_YEARS[planet]
    return Number(age.toFixed(2))
  }

  onMercury(): number { return this.onPlanet('Mercury') }
  onVenus():   number { return this.onPlanet('Venus') }
  onEarth():   number { return this.onPlanet('Earth') }
  onMars():    number { return this.onPlanet('Mars') }
  onJupiter(): number { return this.onPlanet('Jupiter') }
  onSaturn():  number { return this.onPlanet('Saturn') }
  onUranus():  number { return this.onPlanet('Uranus') }
  onNeptune(): number { return this.onPlanet('Neptune') }
}

export default SpaceAge
