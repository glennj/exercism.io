const SECONDS_PER_YEAR: number = 31557600   // seconds per Earth year

const REL_YEARS: {[key: string]: number} = {
  Mercury:   0.2408467,
  Venus:     0.61519726,
  Earth:     1,
  Mars:      1.8808158,
  Jupiter:  11.862615,
  Saturn:   29.447498,
  Uranus:   84.016846,
  Neptune: 164.79132,
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

/*

this is the dynamic stuff I was looking for

    enum Planet {
        Earth = 31557600,
        Mercury = 0.2408467 * Earth,
        Venus = 0.61519726 * Earth,
        Mars = 1.8808158 * Earth,
        Jupiter = 11.862615 * Earth,
        Saturn = 29.447498 * Earth,
        Uranus = 84.016846 * Earth,
        Neptune = 164.79132 * Earth
    }

    export default class SpaceAge {
        [k: string]: any;

        private onPlanet(planet: Planet): number {
            return Math.round(this.seconds * 100 / planet) / 100;
        }

        constructor(readonly seconds: number) {
            for (const planet in Planet) {
                this[`on${planet}`] = () => this.onPlanet(<any>Planet[planet]);
            }
        }
    }

*/