sub norm_date 
{
    my $d = @_[0] ;
    chomp $d ;
    if ( $d =~ /bientot/i ) {return($d);}
    if ( $d =~ /bientôt/i ) {return($d);}
    if ( $d =~ /interdit/i ) {return($d);}
    my $j, $m, $a, $jj, $mm, $aa, $sep ;
    if ( $d =~ m!^(\d{1,2})([ :/\.\-])(\d{1,2})\2(\d{1,4})$! ) {
        $j = &norm_chiffre($1) ; $m = &norm_chiffre($3);
        $a = $4 ;
        if ($a>2000) { $a = $a - 2000 ; }
        $a = &norm_chiffre ($a) ;
        return (join ('/', ($j,$m,$a) ) ) ;
    }
    return ($d) ;
}
sub norm_chiffre {
    my $c = shift ;
    if (length($c)<2) {$c = "0".$c;}
    $c;
}
