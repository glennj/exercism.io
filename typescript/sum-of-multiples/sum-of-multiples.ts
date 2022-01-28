export function sum(factors: number[], limit: number): number {
  return Array.from({length: limit - 1}, (_,i) => i+1)
    .filter(n => factors.some(f => n % f === 0))
    .reduce((sum, multiple) => sum + multiple, 0)
}
