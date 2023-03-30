package grep

// Not using the regexp package: the tests only use plain text patterns

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Options struct {
	lineNumbers     bool // -n
	filenameOnly    bool // -l
	caseInsensitive bool // -i
	inverted        bool // -v
	wholeLine       bool // -x
	multipleFiles   bool
}

func Search(pattern string, flags, files []string) []string {
	var (
		result = []string{}
		opts   = parseOptions(flags)
	)
	if opts.caseInsensitive {
		pattern = strings.ToLower(pattern)
	}
	opts.multipleFiles = len(files) > 1

	for _, file := range files {
		fileResults, err := processFile(file, pattern, opts)
		if err != nil {
			panic(err)
		}

		result = append(result, fileResults...)
	}
	return result
}

func parseOptions(flags []string) (opts Options) {
	for _, flag := range flags {
		switch flag {
		case "-n":
			opts.lineNumbers = true
		case "-l":
			opts.filenameOnly = true
		case "-i":
			opts.caseInsensitive = true
		case "-v":
			opts.inverted = true
		case "-x":
			opts.wholeLine = true
		}
	}
	return opts
}

func processFile(file, pattern string, opts Options) (result []string, err error) {
	lines, err := readLines(file)
	if err != nil {
		return nil, err
	}

	for i, line := range lines {
		if match(pattern, line, opts) {
			if opts.filenameOnly {
				result = append(result, file)
				break
			}
			result = append(result, formatted(line, file, i+1, opts))
		}
	}

	return result, nil
}

func match(pattern, line string, opts Options) (matches bool) {
	if opts.caseInsensitive {
		line = strings.ToLower(line)
	}
	if opts.wholeLine {
		matches = line == pattern
	} else {
		matches = strings.Contains(line, pattern)
	}
	return matches != opts.inverted // logical XOR
}

func formatted(line, file string, lineno int, opts Options) string {
	prefix := ""
	if opts.multipleFiles {
		prefix += file + ":"
	}
	if opts.lineNumbers {
		prefix += strconv.Itoa(lineno) + ":"
	}
	return prefix + line
}

func readLines(file string) ([]string, error) {
	data, err := os.ReadFile(file)
	if err != nil {
		return nil, fmt.Errorf("cannot slurp file '%s': %w", file, err)
	}
	return strings.Split(strings.TrimSuffix(string(data), "\n"), "\n"), nil
}

// bench (limited usefulness, I'm not going to work on alternate implementations)
// BenchmarkSearch             2766            370771 ns/op           75800 B/op        641 allocs/op
