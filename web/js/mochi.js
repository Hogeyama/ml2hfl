function getProgram(name) {
  var uri = "testcases/" + name + ".ml";
  var http = new JKL.ParseXML.Text(uri);
  return http.parse();
}

function init() {
    var editor = ace.edit("editor");
    editor.setFontSize(22);
    editor.getSession().setMode("ace/mode/ocaml");
    editor.getSession().setUseWrapMode
    editor.setOptions({
        maxLines: 35
    });
    editor.getSession().setTabSize(2);
    editor.setValue(getProgram("dummy"), -1); // Please set default program
}

function ex(form) {
    ace.edit("editor").setValue(getProgram(form.program.value), -1);
}

function run() {
    var input = ace.edit("editor").getValue();
    $('#result').html('<h2>Now verifying...</h2>');
    $.post(
        '/~watanabe/cgi-bin/fair_nonterm.cgi',
        {'input':input, 'verbose':$("#verbose").attr("checked")},
        function(txt) { // TODO: fix later
            var idx = txt.search("<span ");
            var result = txt.slice(idx);
            var log = txt.slice(0, idx);
            var out = "<h2>Verification Result</h2>\n";
            out += "<pre>" + result + "</pre>\n";
            if ($("#verbose").prop("checked")) {
                out += "<h2>Log</h2>";
                out += "<pre>" + log + result + "</pre>\n";
            }
            $('#result').html(out);
        }
    );
}

function clear_form(form) {
    form.reset();
    $('#result').text('');
}

$(document).ready(function(){
    $('#demo_body').css('display', 'block');
    init()
})
