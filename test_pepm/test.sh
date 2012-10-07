#!/bin/bash

TEST="sum mult max mc91 ack a-cppr l-zipunzip l-zipmap hors e-simple e-fact r-lock r-file sum-e mult-e mc91-e repeat-e r-lock-e arith_exp sum_intro copy_intro fold_right forall_eq_pair forall_leq isnil iter mem nth0 harmonic fold_left zip map_filter risers fact_notpos fun_list arith_exp-e harmonic-e map_filter-e fact_notpos-e"

LIMIT=600

for i in $TEST
do
echo $i
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -cps-naive -no-enr 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -no-enr 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -cps-naive 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -cps-naive -no-enr -gch 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -no-enr -gch 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -cps-naive -gch 2> /dev/null
timeout -s 14 $LIMIT ./mochi.opt test_pepm/$i.ml -gch 2> /dev/null
echo
done
