use 5.024;
use strictures 2;
use feature 'current_sub';

package BST;
use Class::Tiny qw{ data left right };

sub insert {
    my ($self, $value) = @_;
    my $side = ($value <= $self->data) ? "left" : "right";
    if ($self->{$side}) { $self->{$side}->insert($value); } 
    else                { $self->{$side} = (ref $self)->new(data => $value); }
    return $self;
}

sub toHash {
    my ($self) = @_;
    my $h = {};
    $h->{data} = $self->data;
    $h->{left} = $self->left ? $self->left->toHash() : undef;
    $h->{right} = $self->right ? $self->right->toHash() : undef;
    return $h;
}


## no critic (Subroutines::ProhibitBuiltinHomonyms)
sub each {
    my ($self, $callback) = @_;
    $self->left->each($callback) if $self->left;
    $callback->($self->data);
    $self->right->each($callback) if $self->right;
    return $self;
}

sub sortedData {
    my ($self) = @_;
    my @sorted;
    $self->each(sub {my $data = shift; push @sorted, $data});
    return \@sorted;
}


package BinarySearchTree;
use Exporter::Easiest 'OK => tree treeSort';

sub _bst {
    my @data = (shift)->@*;
    my $root_value = shift @data;
    my $tree = BST->new(data => $root_value);
    $tree->insert($_) for @data;
    return $tree;
}

sub tree {
    return _bst(@_)->toHash;
}

sub treeSort {
    return _bst(@_)->sortedData;
}

1;

