FROM ocaml/opam:ubuntu-16.04_ocaml-4.03.0

USER root
RUN apt-get update && \
    apt-get install -y python autoconf libgmp-dev libmpfr-dev libglpk-dev
USER opam

RUN opam repository set-url default https://opam.ocaml.org/1.2.2 && \
    opam install -y camlp4 camlp5 ocamlfind zarith apron batteries yojson ppx_deriving z3

COPY . .

RUN touch trecs && chmod +x trecs && \
    bash build && \
    LD_LIBRARY_PATH=/home/opam/.opam/4.03.0/lib/z3 ./mochi.opt -v
