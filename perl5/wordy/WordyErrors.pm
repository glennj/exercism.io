package WordyErrors;
use strictures 2;

use Exporter::Easiest 'OK => SyntaxError UnknownOperation';
use Carp;

sub SyntaxError      { croak "syntax error"; }
sub UnknownOperation { croak "unknown operation"; }

1;
