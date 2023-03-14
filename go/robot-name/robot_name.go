package robotname

/* In my experience, the way to make this fast is to generate all 676_000
 * names up front, shuffle them, and pull names off of that list.
 *
 * Another approach, generating a random name and checking if it has been
 * allocated already, is quick at first, but as the number of allocated
 * names gets close to the limit, the process gets really slow.
 */

import (
	"errors"
	"fmt"
	"math/rand"
	//"time"
)

type Robot struct {
	name string
}

var (
	allNames = createNames()
	nameIdx  = 0
)

func (r *Robot) Name() (string, error) {
	if r.name == "" {
		r.Reset()

		if r.name == "" {
			return "", errors.New("all names consumed")
		}
	}

	return r.name, nil
}

func (r *Robot) Reset() {
	if nameIdx >= len(allNames) {
		r.name = ""
		return
	}

	name := allNames[nameIdx]
	nameIdx++
	r.name = name
}

func createNames() []string {
	//startT := time.Now()

	names := make([]string, 26*26*1000)
	idx := 0
	for i := 'A'; i <= 'Z'; i++ {
		for j := 'A'; j <= 'Z'; j++ {
			for k := 0; k < 1000; k++ {
				name := fmt.Sprintf("%c%c%03d", i, j, k)
				names[idx] = name
				idx++
			}
		}
	}
	rand.Shuffle(len(names), func(i, j int) {
		names[i], names[j] = names[j], names[i]
	})

	//duration := time.Now().Sub(startT)
	//fmt.Printf("%d names generated in %v\n", len(names), duration)

	return names
}
