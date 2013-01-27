#!c:\cygwin\bin\perl

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

$cmd = 'ulimit -t 30 -v 100000; ./mochi.opt -gchi -no-enr -margin 80 ' . $filename;
if ($q->param('verbose') ne 'checked') {
    $cmd .= ' -only-result';
}
if ($q->param('complete') ne 'checked') {
    $cmd .= ' -disable-rc';
} else {
    $cmd .= ' -rc';
}

$result = `$cmd`;
$result = CGI::escapeHTML($result);
$result =~ s/Safe!/<span class="h2">Safe!<\/span>/;
$result =~ s/Unsafe!/<span class="h2">Unsafe!<\/span>/;
$result =~ s/Refinement Types:/<span class="h3">Refinement Types:<\/span>/;
$result =~ s/Program with Quantifiers Added:/<span class="h3">Program with <font color="#FF0000">Quantifiers<\/font> Added:<\/span>/;
$result =~ s/\$(.*?)\$/<font color="#FF0000">$1<\/font><\/span>/g;

print $result;

print <<EOF;
</pre>
EOF

if ($result =~ /Safe/) {
    system ("cd $log_dir; caml2html -body " . $base);

    print "<p>Hover the mouse cursor over variables to see refinement types.<p>";

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
