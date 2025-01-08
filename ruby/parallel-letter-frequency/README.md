# Parallel Letter Frequency

Welcome to Parallel Letter Frequency on Exercism's Ruby Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Count the frequency of letters in texts using parallel computation.

Parallelism is about doing things in parallel that can also be done sequentially.
A common example is counting the frequency of letters.
Create a function that returns the total frequency of each letter in a list of texts and that employs parallelism.

## Benchmarking

If you are solving this exercise locally, there is an included `parallel_letter_frequency_benchmark_test.rb` file which will compare your parallel solution to a sequential one.
You can run it via `ruby parallel_letter_frequency_benchmark_test.rb`.
The output will show execution times of each implementation over a range of large text counts in a tabular format.
Feel free to change the test cases.
You may want to investigate what performance impact length of text vs number of texts has on the execution time for each implementation.

**Note:** For smaller sets of text, the sequential count _may_ be faster due to processing overhead costs.

## Further Reading

- [Ruby `Thread` Documentation](https://docs.ruby-lang.org/en/master/Thread.html)
- [Ruby `Thread::Queue` Documentation](https://docs.ruby-lang.org/en/master/Thread/Queue.html)
- [Ruby `Fiber` Documentation](https://docs.ruby-lang.org/en/master/Fiber.html)
- [Ruby `Ractor` Documentation](https://docs.ruby-lang.org/en/master/Ractor.html)
- [`Minitest::Benchmark` Documentation](https://ruby-doc.org/3.0.6/gems/minitest/Minitest/Benchmark.html)

## Source

### Created by

- @mr-sigma
- @kotp