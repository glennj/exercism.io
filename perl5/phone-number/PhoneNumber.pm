## no critic (RegularExpressions::RequireExtendedFormatting)

package PhoneNumber;

use strictures 2;
use Exporter::Easiest 'OK => clean_number';
use Carp;

sub clean_number {
    my ($num) = @_;
    $num =~ s/[-.()]/ /g;   # allowed punctuation
    croak "letters not permitted" if $num =~ /\p{Alpha}/;
    croak "punctuations not permitted" if $num =~ /\p{Punct}/;

    $num =~ s/\D//g;
    croak "incorrect number of digits" if length($num) < 10;
    croak "more than 11 digits" if length($num) > 11;
    croak "11 digits must start with 1"
        if length($num) == 11 && $num !~ /^1/;

    if ($num =~ /^1?(\d{3})(\d{3})(\d{4})$/) {
        my ($area, $exch, $number) = ($1, $2, $3);
        croak "area code cannot start with zero" if $area =~ /^0/;
        croak "area code cannot start with one"  if $area =~ /^1/;
        croak "exchange code cannot start with zero" if $exch =~ /^0/;
        croak "exchange code cannot start with one"  if $exch =~ /^1/;

        return "${area}${exch}${number}";
    }
}

1;
