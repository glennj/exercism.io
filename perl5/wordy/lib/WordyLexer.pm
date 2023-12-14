package WordyLexer;

use 5.024;

#use strictures 2;
use strict;
use warnings;
no warnings 'experimental::smartmatch';     ## no critic (TestingAndDebugging::ProhibitNoWarnings)

use WordyErrors;
use Scalar::Util qw/ looks_like_number /;

############################################################
use Class::Tiny qw/ expression stack state /;

my %Operations = (
    plus       => sub { return (shift) + (shift) },
    minus      => sub { return (shift) - (shift) },
    multiplied => sub { return (shift) * (shift) },
    divided    => sub { return (shift) / (shift) },
);

sub BUILD {
    my ($self) = @_;
    $self->stack([split ' ', $self->expression]);
    $self->state('num');
    return;
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
            SyntaxError->new->raise() unless looks_like_number($token);
            $self->state('op');
            return $token;
        }
        when ('op') {
            # not expecting a number
            SyntaxError->new->raise() if looks_like_number($token);
            UnknownOperationError->new->raise() if not exists $Operations{$token};
            if (grep {$token eq $_} qw/multiplied divided/) {
                UnknownOperationError->new->raise() if $self->_nextToken ne 'by';
            }
            $self->state('num');
            return $Operations{$token};
        }
    }
    return; # should not get here
}

1;
