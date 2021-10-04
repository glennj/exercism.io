package Cipher;

use strictures 2;
use Exporter::Easiest 'OK => encode decode';

# this module simply delegates to AtbashCipher
use lib ('.');
use AtbashCipher;

*encode = *AtbashCipher::encode_atbash;
*decode = *AtbashCipher::decode_atbash;

1;
