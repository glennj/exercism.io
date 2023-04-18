package foodchain

import "strings"

var (
	critters      = []string{"fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"}
	critterAction = map[string]string{
		"spider": "It wriggled and jiggled and tickled inside her.",
		"bird":   "How absurd to swallow a bird!",
		"cat":    "Imagine that, to swallow a cat!",
		"dog":    "What a hog, to swallow a dog!",
		"goat":   "Just opened her throat and swallowed a goat!",
		"cow":    "I don't know how she swallowed a cow!",
		"horse":  "She's dead, of course!",
	}
	critterExtra = map[string]string{
		"spider": " that wriggled and jiggled and tickled inside her",
	}
	whoDunnit = "horse" // spoiler alert!
)

func Verse(v int) string {
	var (
		critter = critters[v-1]
		verse   = strings.Builder{}
	)

	verse.WriteString("I know an old lady who swallowed a ")
	verse.WriteString(critter)
	verse.WriteRune('.')

	if action, ok := critterAction[critter]; ok {
		verse.WriteRune('\n')
		verse.WriteString(action)
	}

	if critter != whoDunnit {
		buildChain(v, &verse)
		verse.WriteRune('\n')
		verse.WriteString("I don't know why she swallowed the fly. Perhaps she'll die.")
	}

	return verse.String()
}

func buildChain(v int, verse *strings.Builder) {
	for i := v - 1; i > 0; i-- {
		verse.WriteRune('\n')
		verse.WriteString("She swallowed the ")
		verse.WriteString(critters[i])
		verse.WriteString(" to catch the ")
		verse.WriteString(critters[i-1])
		verse.WriteString(critterExtra[critters[i-1]])
		verse.WriteRune('.')
	}
}

func Verses(start, end int) string {
	verses := make([]string, 0, end-start+1)
	for v := start; v <= end; v++ {
		verses = append(verses, Verse(v))
	}
	return strings.Join(verses, "\n\n")
}

func Song() string {
	return Verses(1, len(critters))
}

// bench
//
// I'm finding that not using `fmt.Sprintf` and not concatenating strings with `+`
// is reducing the allocs/op and B/op values
//
// BenchmarkSong 	  248278	      4910 ns/op	    8464 B/op	      32 allocs/op
