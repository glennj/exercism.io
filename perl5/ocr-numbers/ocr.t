#!/usr/bin/env perl
use strict;
use warnings;

use Test::More tests => 26;
use Test::Fatal qw(dies_ok);
use FindBin qw($Bin);
use lib $Bin, "$Bin/local/lib/perl5";

my $module = 'OCR';

ok -e "$Bin/$module.pm", "Missing $module.pm" or BAIL_OUT "You need to create file: $module.pm";

eval "use $module";
ok !$@, "Cannot load $module"
        or BAIL_OUT "Cannot load $module. Does it compile? Does it end with 1;?";
can_ok $module, "new"     or BAIL_OUT "Missing package $module; or missing sub new()";
can_ok $module, "convert" or BAIL_OUT "Missing package $module; or missing sub convert()";

###############################
chomp(my $nosep = <<'E1');
 _ 
| |
|_|
   
  |
  |

E1
dies_ok { $module->new($nosep)->convert } 'error: missing separator';

chomp(my $not3chars1 = <<'MISSING_TRAILING_SPACES_FIRST_LINE');
 _
|_ 
 _|
MISSING_TRAILING_SPACES_FIRST_LINE
dies_ok { $module->new($not3chars1)->convert } 'error: uneven lines';

chomp(my $not3chars2 = <<'MISSING_TRAILING_SPACES');
 _ 
|_
 _|
MISSING_TRAILING_SPACES
dies_ok { $module->new($not3chars2)->convert } 'error: uneven lines';

###############################
is $module->new('')->convert, '', "OCR(empty) is empty";

chomp(my $empty1 = <<'Empty1');




Empty1
is $module->new($empty1)->convert, '', "OCR(empty) is empty";

###############################
chomp(my $zero = <<'ZERO');
 _ 
| |
|_|
   
ZERO
is $module->new($zero)->convert, '0', "OCR(0) is 0";

chomp(my $one = <<'ONE');
   
  |
  |
   
ONE
is $module->new($one)->convert, '1', "OCR(1) is 1";

chomp(my $two = <<'TWO');
 _ 
 _|
|_ 
   
TWO
is $module->new($two)->convert, '2', "OCR(2) is 2";

chomp(my $three = <<'THREE');
 _ 
 _|
 _|
   
THREE
is $module->new($three)->convert, '3', "OCR(3) is 3";

chomp(my $four = <<'FOUR');
   
|_|
  |
   
FOUR
is $module->new($four)->convert, '4', "OCR(4) is 4";

chomp(my $five = <<'FIVE');
 _ 
|_ 
 _|
   
FIVE
is $module->new($five)->convert, '5', "OCR(5) is 5";

chomp (my $six = <<'SIX');
 _ 
|_ 
|_|
   
SIX
is $module->new($six)->convert, '6', "OCR(6) is 6";

chomp (my $seven = <<'SEVEN');
 _ 
  |
  |
   
SEVEN
is $module->new($seven)->convert, '7', "OCR(7) is 7";

chomp (my $eight = <<'EIGHT');
 _ 
|_|
|_|
   
EIGHT
is $module->new($eight)->convert, '8', "OCR(8) is 8";

chomp (my $nine = <<'NINE');
 _ 
|_|
 _|
   
NINE
is $module->new($nine)->convert, '9', "OCR(9) is 9";

chomp (my $garble = <<'GARBLE');
   
| |
| |
   
GARBLE
is $module->new($garble)->convert, '?', "garble input is ?";

chomp (my $ten = <<'TEN');
    _ 
  || |
  ||_|
      
TEN
is $module->new($ten)->convert, '10', "OCR(10) is 10";

chomp (my $long_binary = <<'BINARY');
       _     _        _  _ 
  |  || |  || |  |  || || |
  |  ||_|  ||_|  |  ||_||_|
                           
BINARY
is $module->new($long_binary)->convert, '110101100', "convert a long binary number";

chomp (my $mixed = <<'MIXED');
       _     _           _ 
  |  || |  || |     || || |
  |  | _|  ||_|  |  ||_||_|
                           
MIXED
is $module->new($mixed)->convert, '11?10?1?0', "convert 1 and 0 mixed with invalid input";

chomp (my $all = <<'ALL');
    _  _     _  _  _  _  _  _ 
  | _| _||_||_ |_   ||_||_|| |
  ||_  _|  | _||_|  ||_| _||_|
                              
ALL
is $module->new($all)->convert, '1234567890', "all digits";

chomp (my $multiline = <<'MULTILINE');
    _  _ 
  | _| _|
  ||_  _|
         
    _  _ 
|_||_ |_ 
  | _||_|
         
 _  _  _ 
  ||_||_|
  ||_| _|
         
MULTILINE
is $module->new($multiline)->convert, '123,456,789', "multiline input";

chomp (my $multiline2 = <<'MULTILINE2');
   
  |
  |
         
 _  _    
 _| _||_|
|_  _|  |
         
MULTILINE2
is $module->new($multiline2)->convert, '1,234', "multiline input not 3 digits per line";
