package FlowerField;

use v5.42;

use Exporter qw<import>;
our @EXPORT_OK = qw<annotate>;

use constant {
    FLOWER => 99,
    SPACE  => 0
};

sub to_i ($char) {
    return SPACE  if $char eq ' ';
    return FLOWER if $char eq '*';
    die "unexpected character: '$char'";
}

sub to_s ($n) {
    return ' ' if $n == SPACE;
    return '*' if $n >= FLOWER;
    return "$n";
}

sub count_neighbours ($m, $x, $y) {
    my $count = 0;
    for my $dx (-1..1) {
        next if $x + $dx < 0 || $x + $dx >= @$m;
        for my $dy (-1..1) {
            next if $y + $dy < 0 || $y + $dy >= @{$m->[$x + $dx]};
            if ($m->[$x + $dx][$y + $dy] == FLOWER) {
                $count++;
            }
        }
    }
    return $count;
}

# --------------------------------------------------
sub annotate ($garden) {
    my @mtx = map {[map {to_i $_} split "", $_]} (split /\n/, $garden);

    for (my $x = 0; $x < @mtx; $x++) {
        for (my $y = 0; $y < @{$mtx[$x]}; $y++) {
            if ($mtx[$x][$y] != FLOWER) {
                $mtx[$x][$y] = count_neighbours(\@mtx, $x, $y);
            }
        }
    }
    
    return join "\n", (map {join "", map {to_s $_} @$_} @mtx);
}

1;
