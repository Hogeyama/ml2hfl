#!/bin/sh

echo
echo "Naive functor application"
ocamlc -c itype.mli
ocamlc -c ftype.mli
ocamlc -c imod.mli
ocamlc -c fmod.mli
ocamlc -c imod.ml
ocamlc -c fmod.ml
ocamlc -o a.out imod.cmo fmod.cmo main.ml
./a.out <<EOF
4
2
EOF

echo

exit 0
