#!/bin/bash

TEST="sum mult max mc91 ack a-cppr l-zipunzip l-zipmap hors e-simple e-fact r-lock r-file sum_intro copy_intro fact_notpos fold_right forall_eq_pair forall_leq isnil iter length mem nth nth0 harmonic fold_left zip map_filter risers search fold_fun_list fact_notpos-e harmonic-e map_filter-e search-e"

for i in $TEST
do
echo $i
./mochi.opt test_pepm/$i.ml -gchi -only-result 2> /dev/null || echo VERIFICATION FAILED!!!
echo
done
