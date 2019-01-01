/* eslint-disable no-new-func */

const secondsPerEarthYear = 31557600;

const planetaryYears = {
  Mercury: 0.2408467,
  Venus: 0.61519726,
  Earth: 1,
  Mars: 1.8808158,
  Jupiter: 11.862615,
  Saturn: 29.447498,
  Uranus: 84.016846,
  Neptune: 164.79132,
};

// going old school
function SpaceAge(ageInSeconds) {
  this.seconds = ageInSeconds;
  this.round = num => Math.round(100 * num) / 100;
}

Object.keys(planetaryYears).forEach((planet) => {
  SpaceAge.prototype[`on${planet}`] = new Function(`return this.round(this.seconds / ${secondsPerEarthYear * planetaryYears[planet]})`);
});

export { SpaceAge };

/* community notes:

 * number.toFixed(2)
 * bind() --
    Object.keys(PLANET_YEARS).forEach(function(planet_name) {
        SpaceAge.prototype['on'+planet_name] = function(){
            return convert_seconds_to_years(this.seconds,PLANET_YEARS[planet_name] );
        };
    }.bind(SpaceAge));

 * in a class: this[`on${planet}`] = () => ...
      class SpaceAge {
        constructor(timeInSeconds){
          this.seconds = timeInSeconds;

          let periods = {
            onEarth:    1,
            onMercury:  0.2408467,
            onVenus:    0.61519726,
            onMars:     1.8808158,
            onJupiter:  11.862615,
            onSaturn:   29.447498,
            onUranus:   84.016846,
            onNeptune:  164.79132
          };

          for(let p in periods) {
            if(!periods.hasOwnProperty(p)) continue;
            this[p] = () => +(this.seconds / 31557600 / periods[p]).toFixed(2);
          }
        }
      }

  */
