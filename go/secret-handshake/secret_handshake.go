package secret

var actions = []string{
	"wink",
	"double blink",
	"close your eyes",
	"jump",
}

var reverseShake uint = 1 << len(actions)

func Handshake(code uint) []string {
	handshake := make([]string, 0, len(actions))

	for i, a := range actions {
		if bit := code & (1 << i); bit != 0 {
			handshake = append(handshake, a)
		}
	}

	if bit := code & reverseShake; bit != 0 {
		reverse(handshake)
	}

	return handshake
}

func reverse(s []string) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}
