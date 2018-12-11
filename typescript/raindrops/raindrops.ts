class Raindrops {
  convert(num: number): string {
    let drops = ''
    if (num % 3 === 0) { drops += 'Pling' }
    if (num % 5 === 0) { drops += 'Plang' }
    if (num % 7 === 0) { drops += 'Plong' }
    return drops || String(num)
  }
}

export default Raindrops
