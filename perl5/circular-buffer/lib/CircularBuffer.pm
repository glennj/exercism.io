use v5.40;
use experimental qw<class>;

class CircularBuffer;
use Carp;

field $capacity : reader : param;
field $count = 0;
field %ptr   = ( r => 0, w => 0 );
field @data  = (0) x $capacity;

method is_full ()  { $count == $capacity }
method is_empty () { $count == 0 }

method write ($item) {
    croak 'buffer full' if $self->is_full;
    $data[ $ptr{w} ] = $item;
    $self->incrPtr('w');
    $count++;
}

method read () {
    croak 'buffer empty' if $self->is_empty;
    my $item = $data[ $ptr{r} ];
    $self->incrPtr('r');
    $count--;
    return $item;
}

method clear () {
    $count = 0;
    $ptr{w} = $ptr{r};
}

method overwrite ($item) {
    $self->read if $self->is_full;
    $self->write($item);
}

method incrPtr ($p) { $ptr{$p} = ( $ptr{$p} + 1 ) % $capacity }

1;
