#!/bin/sh

echo
echo "Expert functor application"
ocamlc -c itype.mli
ocamlc -c restype.mli
ocamlc -c ftype.mli
ocamlc -c imod.mli
ocamlc -c fmod.mli
ocamlc -c imod.ml
ocamlc -c fmod.ml
ocamlc -c fapply.mli
ocamlc -c fapply.ml
ocamlc -o a.out imod.cmo fmod.cmo fapply.cmo main.ml
./a.out <<EOF
4
2
EOF

echo

exit 0
