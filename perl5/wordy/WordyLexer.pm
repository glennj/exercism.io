package WordyLexer;

use 5.024;
use strictures 2;
no warnings 'experimental::smartmatch';     ## no critic (TestingAndDebugging::ProhibitNoWarnings)

use WordyErrors qw/ SyntaxError UnknownOperation /;
use Scalar::Util qw/ looks_like_number /;

############################################################
use Class::Tiny qw/ stack state /;

my %Operations = (
    plus       => sub { return (shift) + (shift) },
    minus      => sub { return (shift) - (shift) },
    multiplied => sub { return (shift) * (shift) },
    divided    => sub { return (shift) / (shift) },
);

sub BUILDARGS {
    my ($class, $phrase) = @_;
    return {stack => [split ' ', $phrase], state => 'num'}
}

sub _nextToken {
    my ($self) = @_;
    return shift $self->stack->@*;
}

sub next {              ## no critic (Subroutines::ProhibitBuiltinHomonyms)
    my ($self) = @_;
    my $token = $self->_nextToken();
    return unless $token;

    for ($self->state) {
        when ('num') {
            # expecting a number
            SyntaxError() unless looks_like_number($token);
            $self->state('op');
            return $token;
        }
        when ('op') {
            # not expecting a number
            SyntaxError() if looks_like_number($token);
            UnknownOperation() if not exists $Operations{$token};
            if (grep {$token eq $_} qw/multiplied divided/) {
                UnknownOperation() if $self->_nextToken ne 'by';
            }
            $self->state('num');
            return $Operations{$token};
        }
    }
    return; # should not get here
}

1;
