package wordcount

//lint:file-ignore U1000 only one func will actually be used

import (
	"regexp"
	"strings"
	"unicode"
)

type Frequency map[string]int

func WordCount(phrase string) Frequency {
	//return regexMatches(phrase)
	//return stringSplit(phrase)
	return stringFields(phrase)
	//return stateMachine(phrase)
}

// ------------------------------------------------------------
func regexMatches(phrase string) Frequency {
	count := Frequency{}
	pattern := regexp.MustCompile(`[[:alnum:]](['[:alnum:]]*[[:alnum:]])?`)

	for _, word := range pattern.FindAllString(phrase, -1) {
		word = strings.ToLower(word)
		count[word]++
	}
	return count
}

// ------------------------------------------------------------
func stringSplit(phrase string) Frequency {
	// transliterate any non-word character to space
	translit := func(char rune) rune {
		switch {
		case unicode.IsUpper(char):
			return unicode.ToLower(char)
		case unicode.IsLower(char) || unicode.IsDigit(char) || char == '\'':
			return char
		default:
			return ' '
		}
	}
	cleaned := strings.Map(translit, phrase)
	count := Frequency{}

	for _, word := range strings.Split(cleaned, " ") {
		word = strings.Trim(word, "'")
		if len(word) > 0 {
			count[word]++
		}
	}
	return count
}

// ------------------------------------------------------------
func stringFields(phrase string) Frequency {
	separator := func(char rune) bool {
		return !(unicode.IsLetter(char) || unicode.IsDigit(char) || char == '\'')
	}
	count := Frequency{}

	for _, word := range strings.FieldsFunc(phrase, separator) {
		word = strings.Trim(word, "'")
		if len(word) > 0 {
			word = strings.ToLower(word)
			count[word]++
		}
	}
	return count
}

// ------------------------------------------------------------
func stateMachine(phrase string) Frequency {
	var (
		count  = Frequency{}
		chars  = strings.Builder{}
		inWord = false
	)

	for _, char := range phrase {
		if inWord {
			if isAlnum(char) || char == '\'' {
				chars.WriteRune(char)
			} else {
				countWord(&chars, count)
				inWord = false
			}
		} else {
			if isAlnum(char) {
				chars.WriteRune(char)
				inWord = true
			}
		}
	}

	if inWord {
		countWord(&chars, count)
	}

	return count
}

func isAlnum(char rune) bool {
	return '0' <= char && char <= '9' ||
		'a' <= char && char <= 'z' ||
		'A' <= char && char <= 'Z'
}

func countWord(chars *strings.Builder, wordCount Frequency) {
	word := chars.String()
	word = strings.TrimRight(word, "'")
	if len(word) > 0 {
		word = strings.ToLower(word)
		wordCount[word]++
	}
	chars.Reset()
}

/* benchmarks
 *
 * regexp -- most concise but slowest
 * BenchmarkWordCount-2       18735             63966 ns/op           43193 B/op        472 allocs/op
 *
 * string split
 * BenchmarkWordCount-2      144686              7684 ns/op            5176 B/op         48 allocs/op
 *
 * string fields
 * BenchmarkWordCount-2      181827              5902 ns/op            4352 B/op         47 allocs/op
 *
 * state machine
 * BenchmarkWordCount-2      148036              7341 ns/op            3912 B/op         97 allocs/op
 */
