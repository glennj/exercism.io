module LogLevels

let message (logLine: string): string = 
    let idx = logLine.IndexOf("]")
    logLine.Substring(idx + 2).Trim()

let logLevel(logLine: string): string = 
    let idx = logLine.IndexOf("]")
    logLine.Substring(1, idx - 1).ToLower()

let reformat(logLine: string): string = 
    sprintf "%s (%s)" (message logLine) (logLevel logLine)
