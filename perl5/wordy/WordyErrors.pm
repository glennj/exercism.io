## no critic (Modules::ProhibitMultiplePackages)

package WordyErrors;

use strictures 2;
use Carp;
use Class::Tiny qw(message);

sub to_string { return (shift->message); }
sub raise     { croak  (shift->message); }


package SyntaxError;
use parent 'WordyErrors';
use Class::Tiny;
sub BUILD { shift->message("syntax error"); }


package UnknownOperationError;
use parent 'WordyErrors';
use Class::Tiny;
sub BUILD { shift->message("unknown operation"); }

1;
