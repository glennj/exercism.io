package LanguageList;

use v5.38;

our @Languages = ();

sub add_language ($language) {
    push @Languages, $language;
}

sub remove_language () {
    pop @Languages;
}

sub first_language () {
    return $Languages[0];
}

sub last_language () {
    return $Languages[-1];
}

sub get_languages (@elements) {
    return @Languages[map {$_ - 1} @elements];
}

sub has_language ($language) {
    grep {$_ eq $language} @Languages;
}
