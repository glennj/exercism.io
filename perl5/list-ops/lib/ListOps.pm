package ListOps;

use v5.38;

sub append ( $list1, $list2 ) {
    return [ @$list1, @$list2 ];
}

sub foldl ( $func, $acc, $list ) {
    foreach my $e (@$list) {
        $acc = $func->( $acc, $e );
    }
    return $acc;
}

# and all the rest can be composed of append and foldl

sub concat ($lists) {
    return foldl sub ( $acc, $list ) { append $acc, $list }, [], $lists;
}

sub filter ( $func, $list ) {
    return foldl sub ( $acc, $e ) {
        $acc = append $acc, [$e] if $func->($e);
        $acc;
    }, [], $list;
}

sub length ($list) {
    return foldl sub { $_[0] + 1 }, 0, $list;
}

sub map ( $func, $list ) {
    return foldl sub ( $acc, $e ) { append $acc, [ $func->($e) ] }, [], $list;
}

sub reverse ($list) {
    return foldl sub ( $rev, $e ) { append [$e], $rev }, [], $list;
}

sub foldr ( $func, $initial, $list ) {
    return foldl $func, $initial, ListOps::reverse $list;
}

1;
