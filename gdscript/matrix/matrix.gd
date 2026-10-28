class Matrix:
	var rows = []
	var cols = []

	func _init(string):
		rows = Array(string.split("\n")) \
				.map(func(line):
					return Array(line.split(" ")) \
							.map(func(word): return int(word)))

		cols = range(rows[0].size()) \
				.map(func(i): return rows.map(func(row): return row[i]))


	func row(number): return rows[number - 1]

	func column(number): return cols[number - 1]
