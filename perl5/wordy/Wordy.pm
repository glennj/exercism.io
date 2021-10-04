## no critic (RegularExpressions::RequireExtendedFormatting)

package Wordy;

use strictures 2;
use WordyLexer;
use WordyErrors qw/ SyntaxError UnknownOperation /;
use Exporter::Easiest 'OK => answer';

sub answer {
    my $input = shift;

    UnknownOperation() unless $input =~ /^What is\b/;
    SyntaxError() unless $input =~ /\?$/;
    $input =~ s/^What is|\?$//g;

    my $stack = WordyLexer->new($input);
    my $valid = 1;
    my $operation;
    my $token;

    my $result = $stack->next();
    SyntaxError() unless $result;

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

    SyntaxError() if not $valid;
    return $result;
}

1;
