package pascal

func Triangle(n int) (triangle [][]int) {
	if n >= 1 {
		triangle = make([][]int, n)
		for i := 0; i < n; i++ {
			triangle[i] = make([]int, i+1)
			triangle[i][0] = 1
			triangle[i][i] = 1
			for j := 1; j < i; j++ {
				triangle[i][j] = triangle[i-1][j-1] + triangle[i-1][j]
			}
		}
	}
	return triangle
}

// bench
// BenchmarkPascalsTriangleFixed-2        	 1093761	      1096 ns/op	    2224 B/op	      21 allocs/op
// BenchmarkPascalsTriangleIncreasing-2   	  117712	     11987 ns/op	   18088 B/op	     230 allocs/op
