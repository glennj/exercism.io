use v5.40;
use Feature::Compat::Class;

class TwoBucketPuzzle {
    use POSIX qw(fmax);
    use Bucket;
    use Carp;

    field $goal : param;
    field $start : param(start_bucket);
    field $s1 : param(bucket1_size);
    field $s2 : param(bucket2_size);

    field $first;
    field $second;

    # validate
    ADJUST {
        if ( $goal > fmax( $s1, $s2 ) ) {
            croak 'impossible: goal too big';
        }

        my $_gcd = sub ( $m, $n ) {
            ( $m, $n ) = ( $n, $m % $n ) while $n > 0;
            return $m;
        };
        my $gcd = $_gcd->( $s1, $s2 );

        if ( !( $gcd == 1 || $goal % $gcd == 0 ) ) {
            croak 'impossible: goal unsatisfiable';
        }
    }

    # initialize
    ADJUST {
        $first  = Bucket->new( name => 'one', size => $s1 );
        $second = Bucket->new( name => 'two', size => $s2 );
        ( $first, $second ) = ( $second, $first ) if $start eq 'two';
    }

    method solve {
        my ( $moves, $done, $result );

        $moves = 0;
        $first->empty;
        $second->empty;

        if ( $second->size == $goal && $first->size != $goal ) {
            $second->fill;
            $moves++;
        }

        do {
            if    ( $first->is_empty ) { $first->fill; }
            elsif ( $second->is_full ) { $second->empty; }
            else                       { $first->pour_into($second) }
            $moves++;

            ( $done, $result ) = $self->_status($moves);
        } while ( !$done );

        return $result;
    }

    # returns a 2-element list:
    # - `done`, boolean
    # - `result`, hashref
    method _status ($moves) {
        my $result = sub ( $x, $y ) {
            return {
                goalBucket  => $x->name,
                otherBucket => $y->amount,
                moves       => $moves,
            };
        };

        return 1, $result->( $first,  $second ) if $first->amount == $goal;
        return 1, $result->( $second, $first )  if $second->amount == $goal;
        return 0, {};
    }
}

1;
