# Declare package 'HelloWorld'
package HelloWorld;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(hello);

sub hello { 'Hello, World!' }

1;
