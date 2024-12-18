package Isogram;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<is_isogram>;

sub is_isogram ($phrase) {
    my %seen;
    
    # possible, but horrible
    $seen{$_}++ ? return 0 : undef for ((lc $phrase) =~ /[[:alpha:]]/g);

=begin
    my @letters = (lc $phrase) =~ /[[:alpha:]]/g;
    foreach my $letter (@letters) {
        return 0 if $seen{$letter};
        $seen{$letter}++;
    }
=cut

    return 1;
}

1;
