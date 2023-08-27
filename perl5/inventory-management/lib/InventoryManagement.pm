package InventoryManagement;

use v5.38;

sub create_inventory ($items) {
    return add_items({}, $items);
}

sub add_items ( $inventory, $items ) {
    foreach my $item (@$items) {
        $inventory->{$item} += 1;
    }
    return $inventory;
}

sub remove_items ( $inventory, $items ) {
    foreach my $item (@$items) {
        $inventory->{$item}-- if $inventory->{$item} > 0;
    }
    return $inventory;
}

sub delete_item ( $inventory, $item ) {
    delete $inventory->{$item};
    return $inventory;
}
