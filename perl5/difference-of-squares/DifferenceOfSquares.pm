package DifferenceOfSquares;
use strictures 2;
use Exporter::Easiest 'OK => square_of_sum sum_of_squares difference_of_squares';
use List::Util  qw/ sum0 /;


sub square_of_sum {
  my ($number) = @_;
  return (sum0 1..$number) ** 2;
}

sub sum_of_squares {
  my ($number) = @_;
  return sum0 map {$_ ** 2} 1..$number;
}

sub difference_of_squares {
  my ($number) = @_;
  return abs(square_of_sum($number)  -  sum_of_squares($number));
}

1;
