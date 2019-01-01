package RNA;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(to_rna);

sub to_rna {
    my ($dna) = @_;
    $dna =~ tr/GCTA/CGAU/;
    return $dna;
}

1;

__END__
better: 

sub to_rna { shift =~ tr/.../.../r }
