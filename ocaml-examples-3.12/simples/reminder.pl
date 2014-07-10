#!/usr/local/bin/perl
$permanent_trouve = 0;
open(JOUR, $filename) or die "fichier pas trouv� !";
while (my $ligne = <JOUR>)
{
  next if $ligne =~ /^-/; next if $ligne =~ /^#/;
  my ($jour, $n, $mois, $semicol, $prenom, $nom, $prenom_autre, $nom_autre) =
      split(" ",$ligne);
  next if ($jour eq "planning") ;
  next if ($semicol ne ":") ;
  if ($nom)
    {
      if ( ( $dm == $n ) &&
           ( $mon eq $mois ) )
        {
            my $nnn = $prenom ;
            $nnn =~ tr/A-Z/a-z/ ; $nnn =~ tr/�/e/ ; $nnn =~ tr/�/c/ ;
            # ne pas chercher � envoyer de mail les jours f�ri�s
            if ( $nnn eq "ferie" ) { print "Jour f�ri�\n" ; last ; }
            # interroge ma base de donn�es des adresses des permanents
            # et appelle sendmail
            find_mail ($prenom , $nom, $jour, $n, $mois,
                       $prenom_autre, $nom_autre) ;
            if ($prenom_autre) {
                find_mail ($prenom_autre , $nom_autre, $jour, $n, $mois,
                           $prenom , $nom) ;
            }
            $permanent_trouve = 1;
            last;
        }
     }
}
