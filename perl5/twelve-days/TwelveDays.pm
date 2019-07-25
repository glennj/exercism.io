package TwelveDays;
use strictures 2;

sub new { 
    my $class = shift;
    my $self = [
        [],
        ['first',    'a Partridge in a Pear Tree'],
        ['second',   'two Turtle Doves'],
        ['third',    'three French Hens'],
        ['fourth',   'four Calling Birds'],
        ['fifth',    'five Gold Rings'],
        ['sixth',    'six Geese-a-Laying'],
        ['seventh',  'seven Swans-a-Swimming'],
        ['eighth',   'eight Maids-a-Milking'],
        ['ninth',    'nine Ladies Dancing'],
        ['tenth',    'ten Lords-a-Leaping'],
        ['eleventh', 'eleven Pipers Piping'],
        ['twelfth',  'twelve Drummers Drumming'],
    ];
    return bless $self, $class;
}

sub verse {
    my ($self, $n) = @_;
    return 
        "On the $self->[$n][0] day of Christmas my true love gave to me: "
        . join(', ', map {$self->[$_][1]} reverse 2 .. $n)
        . ($n > 1 ? ', and ' : '') 
        . $self->[1][1]
        . ".\n";
}

sub verses {
    my ($self, $from, $to) = @_;
    return join '', map {$self->verse($_) . "\n"} $from .. $to;
}

sub sing { return shift->verses(1, 12) }

1;
