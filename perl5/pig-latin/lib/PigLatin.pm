package PigLatin;

use 5.024;

#use strictures 2;
use strict;
use warnings;
no warnings 'experimental::smartmatch';         ## no critic (TestingAndDebugging::ProhibitNoWarnings)

#use Exporter::Easiest 'OK => translate';
use Exporter qw/ import /;
our @EXPORT_OK = qw/ translate /;

use Carp;


sub translate {
    return join ' ', map { anslatetray($_) } split ' ', shift;
}

sub anslatetray {
    for (lc shift) {
        # apple => appleay, xray => xrayay, yttria => yttriaay
        when ( /^(?: [aeiou] | xr | yt)/x ) { return "${_}ay" }

        # rhythm => ythmrhay
        when ( /^([^aeiouy]+)  (y.+)/x )    { return "${2}${1}ay" }

        # queen => eenquay, square => aresquay
        when ( /^([^aeiou]*qu) (.+)/x )     { return "${2}${1}ay" }

        # strength => engthstray
        when ( /^([^aeiou]+)   (.+)/x )     { return "${2}${1}ay" }

        default                             { croak "Not translated: $_" }
    }
    return;
}

'oneay';                                        ## no critic (Modules::RequireEndWithOne)
