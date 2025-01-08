module ParallelLetterFrequency
  def self.count(texts)
    letterFrequency = proc do |text|
      text.chars
          .select {|c| c =~ /[[:alpha:]]/}
          .map(&:downcase)
          .tally
    end

    freq = Hash.new(0)

    queue = Thread::Queue.new
    threads = texts.map do |text|
      Thread.new { queue << letterFrequency.call(text) }
    end

    texts.count.times { freq.merge!(queue.pop) {|_, a, b| a + b} }

    threads.each(&:join)

    freq
  end
end

=begin
    ractors = texts.map do |text|
      Ractor.new(text) do |t|
        t.chars
         .select {|c| c =~ /[[:alpha:]]/}
         .map(&:downcase)
         .tally
      end
    end
    ractors.reduce(Hash.new(0)) do |freq, ractor|
      freq.merge(ractor.take) {|_, a, b| a + b}
    end
  end
=end
