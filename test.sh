#!/bin/bash

TEST="kmp.ml fact_exn.ml array_init.ml array_max.ml file.ml rev_append.ml rev_accum.ml rev_tricky.ml map_map.ml ack.ml recursive.ml intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml repeat.ml fhnhn2.ml lock.ml neg4.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml repeat2.ml"

TEST="intro1.ml intro2.ml intro3.ml sum.ml mult.ml max.ml mc91.ml ack.ml repeat.ml fhnhn.ml hrec.ml neg.ml a-prod.ml a-cppr.ml a-init.ml l-zipunzip.ml l-zipmap.ml hors.ml e-simple.ml e-fact.ml r-lock.ml r-file.ml sum-e.ml mult-e.ml max-e.ml mc91-e.ml repeat-e.ml lock-e.ml excep-e.ml"

APLAS="fold_right.ml forall_eq_pair.ml forall_leq.ml isnil.ml iter.ml length3.ml mem.ml nth0.ml harmonic.ml fold_left.ml iter_fun_list.ml zip.ml map.ml inits.ml risers.ml"

LIMIT=120
OPTION="-enr -rs"

for i in $APLAS
do
#ls -l test_list/$i
echo $i
timeout -s 14 $LIMIT ./mochi.opt test_list/$i $OPTION | grep afe
echo
done









