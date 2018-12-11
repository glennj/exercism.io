#!perl
use strict;
use warnings;
use Data::Dump 'dd';
use lib '.';
use Kindergarten;

my $full_garden = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV";
my $g = Kindergarten->new($full_garden);
dd $g->garden;

dd $g->student("alice");
dd $g->student("bill");
1;
