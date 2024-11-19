use v5.40;
use Feature::Compat::Class;

class Bucket {
    use POSIX qw(fmin);

    field $name : param : reader;
    field $size : param : reader;
    field $amount : reader = 0;

    method capacity { return $size - $amount; }

    method is_full  { return $amount == $size; }
    method is_empty { return $amount == 0; }

    method fill  { $amount = $size; }
    method empty { $amount = 0; }

    method add ($quantity) { $amount += $quantity; }

    method pour_into ($other) {
        my $quantity = fmin( $self->amount, $other->capacity );
        $self->add( -$quantity );
        $other->add($quantity);
        return;
    }
}

1;
