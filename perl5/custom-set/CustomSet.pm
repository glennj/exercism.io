#!perl

#use strictures 2;
use strict;
use warnings;

use feature qw/ postderef /;

use JSON::PP;

use lib '.';
use CustomSet::Obj;

package CustomSet;

use Exporter qw/ import /;
our @EXPORT_OK = qw/ is_empty_set
                     set_contains
                     is_subset
                     is_disjoint_set
                     is_equal_set
                     add_set_element
                     set_intersection
                     set_difference
                     set_union /;

sub is_empty_set {
    my $data = shift;
    return CustomSet::Obj->new(@$data)->is_empty;
}

sub set_contains {
    my ($data, $item) = @_;
    return CustomSet::Obj->new(@$data)->is_member($item);
}

sub is_subset {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new( @{$lists[1]} )
            ->is_subset( CustomSet::Obj->new(@{$lists[0]}) );
}

sub is_disjoint_set {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new( @{$lists[1]} )
            ->is_disjoint( CustomSet::Obj->new(@{$lists[0]}) );
}

sub is_equal_set {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new( @{$lists[1]} )
            ->is_equal( CustomSet::Obj->new(@{$lists[0]}) );
}

sub add_set_element {
    my ($data, $item) = @_;
    return
        CustomSet::Obj
            ->new(@$data)
            ->add($item)
            ->to_arrayref;
}

sub set_intersection {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new(@{$lists[0]})
            ->intersect(CustomSet::Obj->new(@{$lists[1]}))
            ->to_arrayref;
}

sub set_difference {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new(@{$lists[0]})
            ->difference(CustomSet::Obj->new(@{$lists[1]}))
            ->to_arrayref;
}

sub set_union {
    my (@lists) = @_;
    return 
        CustomSet::Obj
            ->new(@{$lists[0]})
            ->union(CustomSet::Obj->new(@{$lists[1]}))
            ->to_arrayref;
}

1;
