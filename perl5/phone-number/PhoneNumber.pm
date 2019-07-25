package PhoneNumber;
use strictures 2;
use Carp;
use List::Util  qw/ none /;
use Exporter 'import';
our @EXPORT_OK = qw(clean_number);

## no critic (RegularExpressions::RequireExtendedFormatting)

sub clean_number {
    return __PACKAGE__->new(shift)->tonumber;
}

sub new {
    my ($class, $num) = @_;
    my $self = [];
    croak "letters not permitted" if $num =~ /\p{Alpha}/;
    $num =~ s/[-.()]/ /g;   # allowed punctuation
    croak "punctuations not permitted" if $num =~ /\p{Punct}/;

    $num =~ s/\D//g;
    croak "incorrect number of digits" if length($num) < 10;
    croak "more than 11 digits" if length($num) > 11;
    croak "11 digits must start with 1"
        if length($num) == 11 && $num !~ /^1/;

    if ($num =~ /^1?(\d{3})(\d{3})(\d{4})$/) {
        croak "area code cannot start with zero" if $1 =~ /^0/;
        croak "area code cannot start with one"  if $1 =~ /^1/;
        croak "exchange code cannot start with zero" if $2 =~ /^0/;
        croak "exchange code cannot start with one"  if $2 =~ /^1/;

        $self = [$1, $2, $3];  # [area code, exchange, number]
    }
    return bless $self, $class
}

sub tonumber {
    my ($self) = @_;
    return join("", @$self) || undef;
}

sub tostring {
    my ($self) = @_;
    return @$self ? sprintf("1 (%s) %s-%s", @$self) : undef;
}

1;
