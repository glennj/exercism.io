package piglatin

//lint:file-ignore U1000 only one func will actually be used

import (
	"regexp"
	"strings"
)

// order is important below:
var regexes = []*regexp.Regexp{
	regexp.MustCompile(`^()((?:[aeiou]|xr|yt).*)`), // starts with vowel (sound)
	regexp.MustCompile(`^(.?qu)(.*)`),              // starts with q group
	regexp.MustCompile(`^([^aeiou]+)(y.*)`),        // starts with y group
	regexp.MustCompile(`^([^aeiou]+)(.*)`),         // starts with consonants
}
var word = regexp.MustCompile(`\w+`)

func Sentence(sentence string) string {
	return Sentence_fields(sentence)
	//return Sentence_replace(sentence)
}

func Sentence_fields(sentence string) string {
	words := strings.Fields(sentence)
	for i, word := range words {
		words[i] = latinize(word)
	}
	return strings.Join(words, " ")
}

func Sentence_replace(sentence string) string {
	return word.ReplaceAllStringFunc(sentence, latinize)
}

func latinize(word string) string {
	return latinize_find(word)
	//return latinize_match_then_replace(word)
}

func latinize_find(word string) string {
	for _, re := range regexes {
		matches := re.FindStringSubmatch(word)
		if matches != nil {
			return matches[2] + matches[1] + "ay"
		}
	}
	panic("don't know to pig-latinize " + word)
}

func latinize_match_then_replace(word string) string {
	for _, re := range regexes {
		if re.MatchString(word) {
			return re.ReplaceAllString(word, "${2}${1}ay")
		}
	}
	panic("don't know to pig-latinize " + word)
}

// bench
//
// fields + find
// BenchmarkSentence          42523             27970 ns/op            3296 B/op         95 allocs/op
//
// fields + match/replace
// BenchmarkSentence          35652             36762 ns/op            2352 B/op        120 allocs/op
//
// replace + find
// BenchmarkSentence          27998             39361 ns/op            3672 B/op        140 allocs/op
//
// replace + match/replace
// BenchmarkSentence          24950             47985 ns/op            2728 B/op        165 allocs/op
