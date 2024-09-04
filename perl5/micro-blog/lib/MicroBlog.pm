package MicroBlog;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<truncate_post>;

use Encode qw/decode/;

our $MAX_SIZE = 5;

sub truncate_post ($utf8_bytes) {
    my $text = decode('UTF-8', $utf8_bytes);
    return substr($text, 0, $MAX_SIZE);
}

1;
