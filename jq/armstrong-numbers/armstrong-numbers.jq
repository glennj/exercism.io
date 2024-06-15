def digit_count: (log10 | floor) + 1;

def armstrong_sum:
  def _sum:
    if .n == 0
      then .sum
      else .sum += pow(.n % 10; .e) | .n = (.n / 10 | floor) | _sum
    end
  ;
  {n: ., e: digit_count, sum: 0} | _sum
;

.number | . == armstrong_sum