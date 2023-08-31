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
    my ($board) = @_;
    my $rows = [ split /\n/, $board ];
    my $result = MinesweeperBoard->new($rows)->annotate;
    return join "\n", @$result;
}

1;
