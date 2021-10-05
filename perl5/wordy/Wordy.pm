## no critic (RegularExpressions::RequireExtendedFormatting)

package Wordy;

use strictures 2;
use WordyLexer;
use WordyErrors;
use Exporter::Easiest 'OK => answer';

sub answer {
    my $input = shift;

    UnknownOperationError->new->raise() unless $input =~ /^What is\b/;
    SyntaxError->new->raise()           unless $input =~ /\?$/;
    $input =~ s/^What is|\?$//g;

    my $stack = WordyLexer->new(expression => $input);
    my $valid = 1;
    my $operation;
    my $token;

    my $result = $stack->next();
    SyntaxError->new->raise() unless $result;

    while ($token = $stack->next()) {
        if (ref $token eq 'CODE') {
            $operation = $token;
            $valid = 0;
        }
        else {
            $result = $operation->($result, $token);
            $valid = 1;
        }
    }

    SyntaxError->new->raise() if not $valid;
    return $result;
}

1;
