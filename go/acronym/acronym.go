package acronym

import (
	"bytes"
	"regexp"
	"strings"
)

// Abbreviate determines the abbreviation of a string. It finds all the
// words in the string and extracts the first letter of each word.
func Abbreviate(s string) string {
	var result bytes.Buffer
	words := regexp.MustCompile(`\w+`).FindAll([]byte(s), -1)
	for _, word := range words {
		result.WriteByte(word[0])
	}
	return strings.ToUpper(result.String())
}

// also:
//			r := regexp.MustCompile(`(\b\w)`)
//    	m := strings.Join(r.FindAllString(s, -1), "")
// 			return strings.ToUpper(m)
