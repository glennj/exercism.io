package diamond

import (
	"errors"
	"strings"
)

func Gen(char byte) (string, error) {
	if char < 'A' || char > 'Z' {
		return "", errors.New("out of range A-Z")
	}

	var (
		b    byte
		c    byte = 'A'
		size      = int(char - c + 1)
		rows      = make([]string, 2*size-1)
	)

	for i := 0; i < size; i, c = i+1, c+1 {
		row := make([]byte, 2*size-1)
		for j := 0; j < size; j++ {
			b = ' '
			if j == i {
				b = c
			}
			row[(size-1)-j] = b
			row[(size-1)+j] = b
		}
		rows[i] = string(row)
		rows[2*(size-1)-i] = rows[i]
	}

	return strings.Join(rows, "\n"), nil

}
