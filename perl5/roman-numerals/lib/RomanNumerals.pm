package RomanNumerals;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(to_roman);

our %DIGITS = (
  1000 =>  'M', 100 =>  'C', 10 =>  'X', 1 =>  'I',
   900 => 'CM',  90 => 'XC',  9 => 'IX',
   500 =>  'D',  50 =>  'L',  5 =>  'V',
   400 => 'CD',  40 => 'XL',  4 => 'IV',
);

sub to_roman {
    my ($number) = @_;
    my $roman = '';
    for my $inc (reverse sort {$a <=> $b} keys %DIGITS) {
        while ($number >= $inc) {
            $roman  .= $DIGITS{$inc}; 
            $number -= $inc;
        }
    }
    return $roman;
}

1;
