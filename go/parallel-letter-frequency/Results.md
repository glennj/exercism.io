# Test results

I had some perplexing results while developing this solution.
And I based it on Erik's "Getting Started" video, so the quality of the solution _should not_ have been a factor.

## Results

```sh
$ go test --bench=. --benchmem
goos: linux
goarch: amd64
pkg: letter
cpu: AMD EPYC 7542 32-Core Processor
BenchmarkSequentialFrequency        3434            366789 ns/op           17569 B/op         13 allocs/op
BenchmarkConcurrentFrequency        3044            384439 ns/op           12548 B/op         67 allocs/op
PASS
ok      letter  2.517s
```

OK, wtf Go?
The Concurrent solution is _supposed_ to be faster.

## The lightbulb going on ...

I develop some Exercism solutions on a Linode that I ssh into.
What happens when I run this locally on my laptop?

```sh
$ exercism download -t go -e parallel-letter-frequency
$ go test --bench=. --benchmem
goos: darwin
goarch: amd64
pkg: letter
cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
BenchmarkSequentialFrequency
BenchmarkSequentialFrequency-12    	    4542	    230143 ns/op	   17571 B/op	      13 allocs/op
BenchmarkConcurrentFrequency
BenchmarkConcurrentFrequency-12    	   13982	     87974 ns/op	   12550 B/op	      67 allocs/op
PASS
ok  	letter	3.443s
```

D'oh!

## Hooray for multi-core CPUs

I use the cheapest linode available.
Specifically, **1 CPU core**.
My laptop reports having **6 cores**.

Threads (fibers, processes, *goroutines*) are only really helpful when the hardware actually supports parallelism/concurrency.
