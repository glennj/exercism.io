package CryptoSquare;
use strictures 2;
use Exporter::Easiest 'OK => cipher';

use Crypto;

sub cipher {
  my ($text) = @_;
  return Crypto->new($text)->ciphertext;
}

1;
