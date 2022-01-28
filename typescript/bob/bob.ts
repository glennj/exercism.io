export function hey(input: string): string {
  if (isSilence(input))  { return 'Fine. Be that way!' }
  if (isYelling(input))  {
    return isQuestion(input)
      ? "Calm down, I know what I'm doing!"
      : 'Whoa, chill out!'
  }
  if (isQuestion(input)) { return 'Sure.' }
  return 'Whatever.'
}

function isQuestion( input: string ): boolean {
  return /[?]\s*$/.test(input)
}

function isSilence( input: string ): boolean {
  return /^\s*$/.test(input)
}

function isYelling( input: string ): boolean {
  // has an uppercase letter but no lowercase letter.
  return /\p{Lu}/u.test(input) && ! /\p{Ll}/u.test(input)
}
