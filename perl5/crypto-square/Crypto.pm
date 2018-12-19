package Crypto;
use strictures  2;
use POSIX       qw/ ceil /;

use Class::Tiny qw(plaintext);
sub BUILDARGS { return { plaintext => pop } }

sub normalize_plaintext { return lc(shift->plaintext) =~ s/\W//gr }

sub size { return ceil sqrt length shift->normalize_plaintext }

sub plaintext_segments {
    my $self = shift;
    my $size = $self->size;
    return [$self->normalize_plaintext =~ /(.{1,$size})/g];
}

sub ciphertext {
    my $self = shift;
    my @plain_segments = map {[split '']} $self->plaintext_segments->@*;
    my @cipher_segments = map
        { my $i = $_; [map {$_->[$i] // ''} @plain_segments] }
        0 .. $self->size - 1;
    return join '', map {join '', @$_} @cipher_segments;
}

sub normalize_ciphertext {
    my $self = shift;
    my $cipher = $self->ciphertext;
    my $len = length $cipher;
    my $c = $self->size;
    my $r = ceil($len / $c);
    my @c = ($r) x $c;
    $c[-1 - $_]-- for 0 .. ($r * $c - $len) - 1;
    my $re = join '', map {"(.{$_})"} @c;
    return join ' ', ($cipher =~ /$re/);
}

1;
