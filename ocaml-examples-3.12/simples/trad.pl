#!/usr/local/bin/perl

my $file = "mots_islandais";
open LIST, $file || print "ERREUR: $!\n";
my @mots = <LIST>;
close LIST;
my $ln = int(rand($#mots));
chomp($mots[$ln]);
my ($mot,$traduction,$type,$cas) = split ('&',$mots[$ln]);
print "$ln\t$mot ?\n";
my $input = <STDIN>;
print "$traduction\t$type\t$cas\n";
