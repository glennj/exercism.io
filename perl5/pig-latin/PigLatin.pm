package PigLatin;
use strictures 2;
no warnings 'experimental::smartmatch'; 
use feature 'switch';
use Carp;

sub translate {
    return join ' ', map { anslatetray($_) } split ' ', shift;
}

sub anslatetray {
    for (lc shift) {
        # apple => appleay, xray => xrayay, yttria => yttriaay
        when ( /^([aeiou] | xr | yt)/x ) { return "${_}ay" }
        # rhythm => ythmrhay
        when ( /^([^aeiouy]+)  (y.+)/x ) { return "${2}${1}ay" }
        # queen => eenquay, square => aresquay
        when ( /^([^aeiou]*qu) (.+)/x )  { return "${2}${1}ay" }
        # strength => engthstray
        when ( /^([^aeiou]+)   (.+)/x )  { return "${2}${1}ay" }
        #
        default                          { croak "Not translated: $_" }
    }
}

'oneay';
