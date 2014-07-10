#!/usr/local/bin/perl
my $x =  "a\n\n" ;
my $y=chomp $x ;
print "apres un chomp : $x $y\n";
$y=chomp $x ;
print "apres un autre chomp : $x $y\n";
$y=chomp $x ;
print "apres encore un autre chomp : $x $y\n";
