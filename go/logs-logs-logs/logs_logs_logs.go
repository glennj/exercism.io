package logs

import (
	"strings"
	"unicode/utf8"
)

var applicationIdentifiers = map[rune]string{
	'❗': "recommendation",
	'🔍': "search",
	'☀': "weather",
}

// Application identifies the application emitting the given log.
func Application(log string) string {
	for _, char := range log {
		app, exists := applicationIdentifiers[char]
		if exists {
			return app
		}
	}
	return "default"
}

// Replace replaces all occurrences of old with new, returning the modified log
// to the caller.
func Replace(log string, oldRune, newRune rune) string {
	replacer := func(char rune) rune {
		if char == oldRune {
			return newRune
		}
		return char
	}

	return strings.Map(replacer, log)
}

// WithinLimit determines whether or not the number of characters in log is
// within the limit.
func WithinLimit(log string, limit int) bool {
	return utf8.RuneCountInString(log) <= limit
}
