package TwoBucket;

use v5.40;
use Exporter qw<import>;
our @EXPORT_OK = qw<measure>;

use TwoBucketPuzzle;

sub measure ( $bucket1_size, $bucket2_size, $goal, $start_bucket ) {
    return TwoBucketPuzzle->new(
        bucket1_size => $bucket1_size,
        bucket2_size => $bucket2_size,
        goal         => $goal,
        start_bucket => $start_bucket
    )->solve;
}

1;
