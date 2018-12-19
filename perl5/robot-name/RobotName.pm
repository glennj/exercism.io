package RobotName;
use strict;
use warnings;

use Set::Tiny;
our $NAMES = Set::Tiny->new();

sub random_name;

use Class::Tiny qw(name);
sub BUILDARGS  { return {name => random_name} }
sub reset_name {
    my $self = shift;
    my $newname = random_name;
    $NAMES->delete( $self->name );
    return $self->name($newname);
}

sub alpha { return ('A'..'Z')[ int(rand(26)) ] }
sub digit { return int(rand(10)) }
sub random_name {
    my $name;
    do { 
        $name = join "", alpha, alpha, digit, digit, digit 
    } while ($NAMES->contains($name));
    $NAMES->insert($name);
    return $name;
}

1;
