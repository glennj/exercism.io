package cipher

import "strings"

type vigenere struct {
	Key string
}

func NewVigenere(key string) Cipher {
	if !validKey(key) {
		return nil
	}
	return vigenere{Key: key}
}

func (v vigenere) Encode(input string) string {
	return v.encipher(+1, input)
}

func (v vigenere) Decode(input string) string {
	return v.encipher(-1, input)
}

func (v vigenere) encipher(direction int, input string) string {
	var (
		cleaned = cleanInput(input)
		key     = v.keyFor(cleaned)
		result  = strings.Builder{}
	)

	for i, c := range strings.ToLower(cleaned) {
		k := rune(key[i])
		offset := int(c-'a') + direction*int(k-'a')
		e := 'a' + rune(floorMod(offset, 26))
		result.WriteRune(e)
	}
	return result.String()
}

// ------------------------------------------------------------
// the key must be only lowercase letters and not all 'a'
func validKey(key string) bool {
	hasBtoZ := false
	for _, r := range key {
		if r < 'a' || r > 'z' {
			return false
		}
		if r != 'a' {
			hasBtoZ = true
		}
	}
	return hasBtoZ
}

func cleanInput(input string) string {
	cleaned := func(c rune) rune {
		if !('a' <= c && c <= 'z' || 'A' <= c && c <= 'Z') {
			return -1
		}
		return c
	}
	return strings.Map(cleaned, input)
}

func (v vigenere) keyFor(cleaned string) string {
	key := v.Key
	for len(key) < len(cleaned) {
		key += v.Key
	}
	return key
}
