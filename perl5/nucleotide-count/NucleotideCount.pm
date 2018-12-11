package NucleotideCount;

use strict;
use warnings;
use Carp;
use List::Util qw(reduce);
use Exporter 'import';
our @EXPORT_OK = qw(count_nucleotides);

sub count_nucleotides {
  my ($strand) = @_;
  croak "Invalid nucleotide in strand" if $strand =~ /[^AGCT]/;
  my $count = {A => 0, G => 0, C => 0, T => 0};
  return reduce { $a->{$b}++; $a } $count, split //, $strand;
}

1;
