package parsinglogfiles

import "regexp"

func IsValidLine(text string) bool {
	valid, _ := regexp.MatchString(`^\[(?:TRC|DBG|INF|WRN|ERR|FTL)\]`, text)
	return valid
}

func SplitLogLine(text string) []string {
	re := regexp.MustCompile(`<[~*=-]*>`)
	return re.Split(text, -1)
}

func CountQuotedPasswords(lines []string) int {
	re := regexp.MustCompile(`(?i:"[^"]*password[^"]*")`)
	var count int
	for _, line := range lines {
		if re.MatchString(line) {
			count++
		}
	}
	return count

	/* Alternately, split each line on double quotes,
	 * and check every _other_ field for `password`.
	 * I'm guessing this approach will be slower due to
	 * greater memory requirements.
	 */
}

func RemoveEndOfLineText(text string) string {
	re := regexp.MustCompile(`end-of-line\d+`)
	return re.ReplaceAllLiteralString(text, "")
}

func TagWithUserName(lines []string) []string {
	tagged := make([]string, 0, len(lines))

	/* one approach: capture the substring
	re := regexp.MustCompile(`\bUser\s+(\S+)`)
	for _, line := range lines {
		submatch := re.FindStringSubmatch(line)
		if submatch != nil {
			line = "[USR] " + submatch[1] + " " + line
		}
		tagged = append(tagged, line)
	}
	*/

	// tidier
	re := regexp.MustCompile(`(.*?\sUser\s+(?P<name>\S+))`)
	for _, line := range lines {
		tagged = append(tagged, re.ReplaceAllString(line, "[USR] $name $1"))
	}

	return tagged
}
