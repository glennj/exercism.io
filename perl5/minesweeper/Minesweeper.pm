package Minesweeper;

use 5.024;

#use strictures 2;
use strict;
use warnings;

#use Exporter::Easiest 'OK => annotate';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ annotate /;

use MinesweeperBoard;

sub annotate {
    my ($rows) = @_;
    return MinesweeperBoard->new($rows)->annotate;
}

1;
