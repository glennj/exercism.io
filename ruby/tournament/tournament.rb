class Tournament
  private

  attr_reader :standings, :data

  TABLE_FMT = "%-30s | %2s | %2s | %2s | %2s | %2s\n".freeze

  public

  def self.tally(data)
    new(data).to_s
  end

  def initialize(data)
    @standings = Hash.new { |h, k| h[k] = Hash.new(0) }
    @data = data
  end

  def to_s
    record_results data
    tabulate
  end

  private

  def record_results(input)
    input.lines.each do |line|
      m = line.match(/^(.+?);(.+?);(.+)$/)
      record(*m.captures) if m
    end
    self
  end

  def record(home, away, result)
    case result
    when 'win', 'loss', 'draw'
      standings[home][:mp] += 1
      standings[away][:mp] += 1
      # dispatch to the method
      send(result, home, away)
    else
      raise ArgumentError, "unknown result: '#{result}' - should be 'win', 'loss', or 'draw'"
    end
  end

  def win(home, away)
    standings[home][:w] += 1
    standings[home][:p] += 3
    standings[away][:l] += 1
  end

  def loss(home, away)
    standings[away][:w] += 1
    standings[away][:p] += 3
    standings[home][:l] += 1
  end

  def draw(home, away)
    standings[home][:d] += 1
    standings[home][:p] += 1
    standings[away][:d] += 1
    standings[away][:p] += 1
  end

  # sort by points descending and team name ascending
  def teams_sorted
    standings.keys.sort_by { |t| [-standings[t][:p], t] }
  end

  def tabulate
    result = format TABLE_FMT, 'Team', 'MP', 'W', 'D', 'L', 'P'
    teams_sorted.each { |team| result << team_results(team) }
    result
  end

  def team_results(team)
    format TABLE_FMT, team, *%i[mp w d l p].map { |i| standings[team][i].to_s }
  end
end
