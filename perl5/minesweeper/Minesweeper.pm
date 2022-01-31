package Minesweeper;

use 5.024;
use strictures 2;
use MinesweeperBoard;
use Exporter::Easiest 'OK => annotate';

sub annotate {
    my ($rows) = @_;
    return MinesweeperBoard->new($rows)->annotate;
}

1;
