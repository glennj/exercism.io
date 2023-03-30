package matrix

import (
	"fmt"
	"strconv"
	"strings"
)

type Matrix [][]int

func New(s string) (Matrix, error) {
	var (
		lines = strings.Split(s, "\n")
		m     = make(Matrix, len(lines))
		ncols int
	)

	for i, line := range lines {
		fields := strings.Fields(line)
		if i == 0 {
			ncols = len(fields)
		} else if len(fields) != ncols {
			return nil, fmt.Errorf("wrong number of fields in '%s': expecting %d", line, ncols)
		}

		m[i] = make([]int, ncols)
		for j, field := range fields {
			n, err := strconv.Atoi(field)
			if err != nil {
				return nil, fmt.Errorf("parse error in line '%s': %w", line, err)
			}

			m[i][j] = n
		}
	}
	return m, nil
}

// Cols and Rows must return the results without affecting the matrix.
func (m Matrix) Cols() [][]int {
	var (
		nrows = len(m)
		ncols = len(m[0])
		cols  = make([][]int, ncols)
	)

	for c := 0; c < ncols; c++ {
		cols[c] = make([]int, nrows)
		for r := 0; r < nrows; r++ {
			cols[c][r] = m[r][c]
		}
	}
	return cols
}

func (m Matrix) Rows() [][]int {
	var (
		nrows = len(m)
		ncols = len(m[0])
		rows  = make([][]int, nrows)
	)

	for r := 0; r < nrows; r++ {
		rows[r] = make([]int, ncols)
		for c := 0; c < ncols; c++ {
			rows[r][c] = m[r][c]
		}
	}
	return rows
}

func (m Matrix) Set(row, col, val int) (ok bool) {
	if 0 <= row && row < len(m) && 0 <= col && col < len(m[0]) {
		ok = true
		m[row][col] = val
	}
	return
}

/* bench
 *
 * BenchmarkNew      930250              1240 ns/op             672 B/op         10 allocs/op
 * BenchmarkRows    4067924               318.2 ns/op           192 B/op          5 allocs/op
 * BenchmarkCols    3246522               369.3 ns/op           288 B/op          6 allocs/op
 */
