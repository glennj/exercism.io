class HandShake {
  private readonly secret: number
  constructor(secret: number) { this.secret = secret }

  // Map preserves insertion order
  static actions: Map<number, string> = new Map([
    [0b00001, 'wink'],
    [0b00010, 'double blink'],
    [0b00100, 'close your eyes'],
    [0b01000, 'jump'],
  ])
  static reverse = 0b10000

  commands(): string[] {
    const commands: string[] = []
    for (const [flag, action] of HandShake.actions) {
      if (this.secret & flag) {
        commands.push(action)
      }
    }
    return this.secret & HandShake.reverse
      ? commands.reverse()
      : commands
  }
}

export function commands(code: number): string[] {
  return new HandShake(code).commands()
}
