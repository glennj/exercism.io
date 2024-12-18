package Acronym;

use v5.40;

use Exporter qw<import>;
our @EXPORT_OK = qw<abbreviate>;

sub abbreviate ($phrase) {
    # search for letters that follow either
    # - the start of the string, or
    # - a non-word character (here, "word" chars are letters and apostrophe)
    #
    # Because variable-length lookbehinds aren't allowed,
    # use \K to "forget" the previous matched text.
    
    my @letters = $phrase =~ / (?: ^ | [^'[:alpha:]] ) \K ([[:alpha:]]) /xg;
    return uc join '', @letters;
}

1;
