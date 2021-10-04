package GradeSchool;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(roster);

sub roster {
    my ($students, $grade) = @_;
    return [
        map  {$_->[0]} 
        grep {defined $grade ? $_->[1] == $grade : 1} 
        sort {$a->[1] <=> $b->[1] or $a->[0] cmp $b->[0]}
        @$students
    ];
}

1;
