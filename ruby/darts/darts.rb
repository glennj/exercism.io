class Darts
  def initialize(x, y)
    @dist = Math.hypot(x, y)
  end

  def score
    case
    when @dist <=  1 then 10
    when @dist <=  5 then  5
    when @dist <= 10 then  1
    else                   0
    end
  end
end
