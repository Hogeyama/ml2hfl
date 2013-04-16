#!/usr/bin/perl

use CGI;
$q = new CGI;

print <<EOF;
Content-type: text/plain

<h2>Verification result</h2>
<pre>
EOF

($sec,$min,$hour,$mday,$mon,$year,$wno) = localtime(time);
$date = sprintf("%04d%02d%02d_%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);

$base = sprintf("%s_%08d.ml",$date,rand(100000000));
$log_dir = "./log/";
$filename = $log_dir . $base;
open FILE, '>', $filename;
print FILE $q->param('input');
close FILE;

if ($q->param('verbose') ne 'checked') {
    $cmd = "(ulimit -t 3 -v 100000; /home/ryosuke/repos/mochi/mochi.opt -margin 80 $filename -only-result) || (ulimit -t 30 -v 100000; /home/ryosuke/repos/mochi/mochi.opt -gchi -margin 80 $filename -only-result) || echo TIMEOUT";
}
else {
    $cmd = "(ulimit -t 3 -v 100000; /home/ryosuke/repos/mochi/mochi.opt -margin 80 $filename) || (ulimit -t 30 -v 100000; /home/ryosuke/repos/mochi/mochi.opt -gchi -margin 80 $filename) || echo TIMEOUT";
}

$result = `$cmd`;
$result = CGI::escapeHTML($result);
$result =~ s/Safe!/<span class="h2">Safe!<\/span>/;
$result =~ s/Unsafe!/<span class="h2">Unsafe!<\/span>/;
$result =~ s/Refinement Types:/<span class="h3">Refinement Types:<\/span>/;

print $result;

print <<EOF;
</pre>
EOF

if ($result =~ /Safe/) {
    system ("cd $log_dir; ./caml2html -body " . $base);

    print "<p>Hover the mouse cursor over variables to see refinement types.</p>";

    open FILE, '<', ($filename . ".html");
    while ($line = <FILE>) {
        print $line;
    }

print <<EOF;
<hr>
<p>
<em>This document was generated using
<a href="http://martin.jambon.free.fr/caml2html.html">caml2html</a></em>
</p>
EOF
}
