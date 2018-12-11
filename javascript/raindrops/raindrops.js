/* eslint-disable class-methods-use-this */

const isFactor = (i, n) => {
  const j = n / i;
  return j === Math.floor(j);
};

export default class Raindrops {
  convert(n) {
    return [
      [3, 'Pling'],
      [5, 'Plang'],
      [7, 'Plong'],
    ].reduce((rain, [f, s]) => rain + (isFactor(f, n) ? s : ''), '')
    || n.toString();
  }
}

/* community
 *
 * well shit, modulo

    function Raindrops(){
      this.convert = function(number){
        var result = '';
        if(number % 3 === 0) result += 'Pling';
        if(number % 5 === 0) result += 'Plang'; 
        if(number % 7 === 0) result += 'Plong';
        return result || number.toString();
      }
    }
    module.exports = Raindrops;

 *
 */
