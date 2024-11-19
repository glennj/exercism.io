package RunLengthEncoding;

## no critic (RequireFinalReturn)

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<encode decode>;

sub encode ($string) { $string =~ s/   (.) \1+ / length($&) . $1 /xegr }
sub decode ($string) { $string =~ s/ (\d+) (.) /         $2 x $1 /xegr }

1;
