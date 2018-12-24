package PhoneNumber;
use strict;
use warnings;
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
    $num =~ s/\D//g;
    if ($num =~ /^1?(\d{3})(\d{3})(\d{4})$/ and none {/^[01]/} ($1, $2)) {
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
    return @$self ? sprintf("+1 (%s) %s-%s", @$self) : undef;
}

1;
