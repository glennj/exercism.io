package forth

import (
	"fmt"
	"strconv"
)

type Macros struct {
	m map[string][]string
}

func NewMacros() *Macros {
	return &Macros{m: map[string][]string{}}
}

func (m *Macros) Put(name string, macro []string) {
	m.m[name] = macro
}

func (m *Macros) Get(name string) (macro []string, ok bool) {
	macro, ok = m.m[name]
	return
}

func (m *Macros) Record(tokens []string) error {
	if len(tokens) < 2 || tokens[len(tokens)-1] != ";" {
		return fmt.Errorf("malformed macro")
	}

	name, words := tokens[0], tokens[1:len(tokens)-1]
	if _, err := strconv.Atoi(name); err == nil {
		return fmt.Errorf("numbers not allowed")
	}

	macro := []string{}
	for _, word := range words {
		defn, ok := m.Get(word)
		if ok {
			macro = append(macro, defn...)
		} else {
			macro = append(macro, word)
		}
	}
	m.Put(name, macro)
	return nil
}

// benchmarked
// BenchmarkForth     28886             43342 ns/op           10480 B/op        359 allocs/op
