package chessboard

// type File stores if a square is occupied by a piece.
type File []bool

// type Chessboard contains a map of eight Files, accessed with keys from "A" to "H".
type Chessboard map[string]File

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) int {
	aFile, exists := cb[file]
	if !exists {
		return 0
	}

	count := 0
	for _, occupied := range aFile {
		if occupied {
			count++
		}
	}
	return count
}

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) int {
	if !(1 <= rank && rank <= len(cb)) {
		return 0
	}

	count := 0
	for _, aFile := range cb {
		if aFile[rank-1] {
			count++
		}
	}
	return count
}

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) int {
	count := 0
	for _, v := range cb {
		count += len(v)
	}
	return count
}

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) int {
	count := 0
	for file := range cb {
		count += CountInFile(cb, file)
	}
	return count
}
