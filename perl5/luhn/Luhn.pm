package Luhn;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(is_luhn_valid);
use List::Util   qw( sum );

sub is_luhn_valid {
    my ($input) = @_;
    $input =~ s/\s//g;
    return if $input =~ /\D/ or $input eq '0';
    my $i = 0;
    my $sum = sum map { digit_handler($_, $i++) } reverse split '', $input;
    return $sum % 10 == 0;
}

sub digit_handler {
    my ($n, $i) = @_;
    $n *= 2 ** ($i % 2);
    return $n > 9 ? $n - 9 : $n;
}

1;
