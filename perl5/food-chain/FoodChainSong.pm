package FoodChainSong;
use strictures 2;

our @CHAIN = ( {},
    { name => 'fly' },
    { name => 'spider', tag => 'It wriggled and jiggled and tickled inside her.',
                        extra => ' that wriggled and jiggled and tickled inside her' },
    { name => 'bird',   tag => 'How absurd to swallow a bird!' },
    { name => 'cat',    tag => 'Imagine that, to swallow a cat!' },
    { name => 'dog',    tag => 'What a hog, to swallow a dog!' },
    { name => 'goat',   tag => 'Just opened her throat and swallowed a goat!' },
    { name => 'cow',    tag => "I don't know how she swallowed a cow!" },
    { name => 'horse',  tag => "She's dead, of course!" },
);

sub new { return bless {}, shift }

sub verse {
    my ($self, $n) = @_;
    my $verse = _iknow($CHAIN[$n]);
    if ($n < $#CHAIN) {
        for (my $i = $n; $i >= 2; $i--) {
            $verse .= _hunt($CHAIN[$i], $CHAIN[$i - 1]);
        }
        $verse .= _ending();
    }
    return $verse;
}

sub verses {
    my ($self, $from, $to) = @_;
    return join '', map {$self->verse($_) . "\n"} $from .. $to;
}

sub sing {
    my $self = shift;
    return $self->verses(1, $#CHAIN);
}

sub _iknow  { 
    my $animal = shift;
    return sprintf "I know an old lady who swallowed a %s.\n%s",
        $animal->{name},
        (exists $animal->{tag} ? "$animal->{tag}\n" : "");
}

sub _hunt   { 
    my ($predator, $prey) = @_;
    return sprintf "She swallowed the %s to catch the %s%s.\n", 
        $predator->{name},
        $prey->{name},
        $prey->{extra} // '';
}

sub _ending { "I don't know why she swallowed the fly. Perhaps she'll die.\n" }

1;
