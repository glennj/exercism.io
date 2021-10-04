package Accumulate;
use strictures 2;
use Exporter::Easiest 'OK => accumulate';

sub accumulate {
    my ($input, $func) = @_;
    $input //= [];
    $func  //= sub { };
    my @result;
    push @result, $func->($_) foreach @$input;
    return \@result;
}

1;
