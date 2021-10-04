## no critic (RegularExpressions::RequireExtendedFormatting)

package Crypto;

use 5.024;
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
    return '' if $self->plaintext eq '';
    my @plain_segments = map {[split '']} $self->plaintext_segments->@*;
    my @cipher_segments = ();
    for my $i (0 .. $self->size - 1) {
        push @cipher_segments, [map {$_->[$i] // ' '} @plain_segments]
    }
    return join ' ', map {join '', @$_} @cipher_segments;
}

1;
