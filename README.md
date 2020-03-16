

Dependency
==========

- re2
- fmt
- str
- unix
- yojson
- batteries
- compiler-libs.common

All of them can be installed via opam.

Build
=====

```
$ cp parser_wrapper_[YOUR-VERSION-OF-OCAML].ml parser_wrapper.ml
$ dune build ./ml2hfl.exe
```

Usage
=====

```
dune exec ./ml2hfl.exe INPUT
```

Note
====

This project reuses code of [MoCHi](http://www-kb.is.s.u-tokyo.ac.jp/~ryosuke/mochi/).
Rest of this README is MoCHi's original one.

--------------------------------------------------------------------------------

How to build MoCHi
==================

 Install the required tools/libraries listed below,
 and run "bash build", which generates "mochi.opt".


What do you need?
=================

- OCaml compiler (from 4.03 to 4.08; 4.04.2 is recommended)
- Libraries available via OPAM
  - ocamlfind/findlib
  - Z3 4.7.1
  - ppx_deriving
  - Yojson
  - batteries
  - camlp4
  - camlp5
  - zarith
  - apron
- HorSat2 binary (https://github.com/hopv/horsat2)


Dockerfile
==========

 There is a Dockerfile for compiling MoCHi.
 Dockerfile assumes the HorSat2 binary is in the same directory.


Licenses
========

 This software is licensed under the Apache License, Version2.0 (http://www.apache.org/licenses/LICENSE-2.0.txt).

 The software uses the following libraries/tools.
 Please also confirm each license of the library/tool.
- CSIsat (https://github.com/dzufferey/csisat)
- ATP (http://www.cl.cam.ac.uk/~jrh13/atp/)


Author
=======

 MoCHi is developed/maintained by Ryosuke SATO <sato@ait.kyushu-u.ac.jp>


Contributors
============

- Hiroshi Unno
- Takuya Kuwahara
- Keiichi Watanabe
- Naoki Iwayama
