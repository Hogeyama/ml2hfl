#!/bin/bash

TEST="kmp fact_exn array_init array_max file rev_append rev_accum rev_tricky map_map ack recursive intro1 intro2 intro3 sum mult max mc91 repeat fhnhn2 lock neg4 sum-e mult-e max-e mc91-e repeat-e lock-e repeat2"

TEST="intro1 intro2 intro3 sum mult max mc91 ack repeat fhnhn hrec neg a-prod a-cppr a-init l-zipunzip l-zipmap hors e-simple e-fact r-lock r-file sum-e mult-e max-e mc91-e repeat-e lock-e excep-e"

APLAS="fold_right forall_eq_pair forall_leq isnil iter mem nth0 harmonic fold_left zip risers inits length3 map iter_fun_list"

LIMIT=120
OPTION="-enr -rsn 0"

for i in $APLAS
do
#ls -l test_list/$i
echo $i
timeout -s 14 $LIMIT ./mochi.opt test_list/$i.ml $OPTION | grep afe
echo
done









