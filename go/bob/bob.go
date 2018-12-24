package bob

import "strings"

func hasAlpha(s string) bool {
	return strings.ContainsAny(strings.ToUpper(s), "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
}

// Hey should have a comment documenting it.
func Hey(remark string) string {
	isQuestion := remark[len(remark)-1] == '?'
	isShouting := hasAlpha(remark) && remark == strings.ToUpper(remark)
	isSilent := len(strings.Fields(remark)) == 0

	if isSilent {
		return "Fine. Be that way!"
	}
	if isQuestion {
		if isShouting {
			return "Calm down, I know what I'm doing!"
		}		else {
			return "Sure."
		}
	}
	if isShouting {
		return "Whoa, chill out!"
	}	else {
		return "Whatever."
	}
}
